module MidiTest

open WebMIDI

type Alert =
  | Info of string
  | Success of string
  | Warning of string
  | Error of string

type Model =  { MIDIOutputs: (string*string) list
                SelectedMIDIOutput: string option
                MIDIAccess: IMIDIAccess option
                IsMIDIEnabled: bool
                Messages: Alert list 
                Notes: Map<byte, byte>
                NotesSustained: Map<byte, byte> option
                }

type NoteMsg =
    | NoteOn of byte * byte
    | NoteOff of byte * byte
    | SustainOn
    | SustainOff

type Msg = 
  | MIDIConnected of IMIDIAccess     // MIDI successfully connected
  | MIDIStateChange                  // MIDI successfully connected
  | MIDIError of exn                 // Error connecting MIDI
  | Message of Alert                 // A message
  | OutputSelected of string
  | SendNote                        // Send a MIDI note
  | ReceiveNote of NoteMsg

module Notes = 
    type Note = 
        | C
        | CSharp
        | D
        | DSharp
        | E
        | F
        | FSharp
        | G
        | GSharp
        | A
        | ASharp
        | B
    let (|IsOneOf|_|) (x: byte) n =
        if [1..10] |> List.map(fun l -> ((12uy * (l |> byte)) + x) |> byte ) |> List.contains n then
            Some ()
        else None
    let toNote = function
        | IsOneOf 0uy -> C
        | IsOneOf 1uy -> CSharp
        | IsOneOf 2uy -> D
        | IsOneOf 3uy -> DSharp
        | IsOneOf 4uy -> E
        | IsOneOf 5uy -> F
        | IsOneOf 6uy -> FSharp
        | IsOneOf 7uy -> G
        | IsOneOf 8uy -> GSharp
        | IsOneOf 9uy -> A
        | IsOneOf 10uy -> ASharp
        | IsOneOf 11uy -> B
        | _ -> C

    let getRoot (l: byte list)= 
        if l.Length > 0 then l |> List.min |> Some else None

    let offsetAgainstRoot (root: byte) l =
        l |> List.map (fun l -> l - root)
open Elmish
open Fable.Import
open Fable.Helpers.React

let init () : Model*Cmd<Msg> =
    { MIDIOutputs = []
      SelectedMIDIOutput = None
      MIDIAccess = None
      IsMIDIEnabled = false
      Messages = [] 
      Notes = Map.empty
      NotesSustained = None }, Cmd.ofPromise MIDI.requestAccess [ Sysex true ] MIDIConnected MIDIError

[<RequireQualifiedAccess>]
module JSMap =
    let toList (m: JS.Map<'key, 'value>): ('key * 'value) list =
        let mutable result = []
        m.forEach (fun value key _ -> result <- (key,value)::result) 
        result

let sendNote (midiAccess: IMIDIAccess) portId =
    let output = midiAccess.outputs.get(portId);
    
    // note on, middle C, full velocity
    let noteOnMessage = [| 0x90uy; 60uy; 0x7fuy |]
    
    // note off, middle C, release velocity = 64 
    let noteOffMessage = [| 0x80uy; 60uy; 0x40uy |]
    
    //omitting the timestamp means send immediately.
    output.send noteOnMessage   
    
    // timestamp = now + 1000ms.
    noteOffMessage |> output.SendAt (Browser.window.performance.now() + 1000.0)

let processBytes dispatch (data: byte array)  = 
    match data with
    | [|248uy|] -> ()
    | [|144uy; note; velocity|] -> NoteOn(note, velocity) |> dispatch //Fable.Import.Browser.console.log ("Note on", note, velocity)
    | [|128uy; note; velocity|] -> NoteOff(note, velocity) |> dispatch//Fable.Import.Browser.console.log ("Note off", note, velocity)
    | [|176uy; 64uy; 127uy|] -> SustainOn |> dispatch
    | [|176uy; 64uy; 0uy|] -> SustainOff |> dispatch
    | x -> Fable.Import.Browser.console.log x
 
let update (msg:Msg) (model:Model) : Model*Cmd<Msg> =    
    let success = Success >> Message >> Cmd.ofMsg
    let info = Info >> Message >> Cmd.ofMsg
    let error = Error >> Message >> Cmd.ofMsg
    
    match msg with
    | MIDIConnected midiAccess -> 
        let stateChangeSub dispatch =
            midiAccess.onstatechange <- (fun (ev:IMIDIConnectionEvent) -> (dispatch MIDIStateChange))
            midiAccess.inputs 
            |> JSMap.toList 
            |> List.map(fun (k, v: IMIDIInput) -> v.onmidimessage <- (fun d -> d.data |> processBytes (ReceiveNote >> dispatch) ))
            |> ignore
        { model with MIDIAccess = Some midiAccess
                     IsMIDIEnabled = true }, Cmd.batch [ success "MIDI connected"
                                                         Cmd.ofSub stateChangeSub
                                                         Cmd.ofMsg MIDIStateChange ]
    | MIDIStateChange ->
        let outputs = 
            match model.MIDIAccess with
            | Some midiAccess ->
                
                midiAccess.outputs 
                |> JSMap.toList 
                |> List.map (fun (key, o) -> key, (o.name |> Option.defaultValue "?")) 
            | None -> []
        
        let selectedOutput = 
            match outputs with
            | (key, _)::_ -> Some key
            | _ -> None

        { model with MIDIOutputs = outputs
                     SelectedMIDIOutput = selectedOutput }, info "State changed"

    | MIDIError ex ->
        { model with MIDIAccess = None
                     MIDIOutputs = []
                     IsMIDIEnabled = false
                     SelectedMIDIOutput = None }, error ex.Message
    | Message alert -> { model with Messages = alert :: model.Messages |> List.truncate 5 }, Cmd.none
    | OutputSelected id ->
        { model with SelectedMIDIOutput = match id with 
                                          | "" -> None 
                                          | id -> Some id }, Cmd.none
    | SendNote -> 
        match model.MIDIAccess, model.SelectedMIDIOutput with
        | Some midi, Some out -> model, Cmd.ofFunc (sendNote midi) out (fun _ -> Message (Success "sent")) (fun ex -> Message (Error ex.Message))
        | Some _, None -> model, error "No Output"
        | _, _ -> model, error "No MIDI connection"
    | ReceiveNote n ->
        let mNext = 
            match n with
            | NoteOn (n, v) -> {model with Notes = model.Notes |> Map.add n v}
            | NoteOff (n, v) -> 
                match model.NotesSustained with
                | Some sustained ->
                    {model with Notes = model.Notes |> Map.remove n; NotesSustained = sustained |> Map.add n 64uy |> Some }
                | None ->
                    {model with Notes = model.Notes |> Map.remove n}
            | SustainOn -> {model with NotesSustained = Some Map.empty}
            | SustainOff -> {model with NotesSustained = None }
        mNext, Cmd.none

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop

let view model dispatch =
    div [ ClassName "container" ] [
        div [ ClassName "row" ] [
            div [ ClassName "col" ] [
                div [ ClassName "card" ] [
                    div [ ClassName "card-header" ] [ strong [] [ str "MIDI Test"] ]
                    div [ ClassName "card-body" ] [
                        div [ ClassName "form-group" ] [
                            label [ ClassName "col-form-label" ] [ str "Outputs" ]
                            select [ ClassName "form-control" 
                                     Value (model.SelectedMIDIOutput |> Option.defaultValue "") 
                                     OnChange (fun (ev:React.FormEvent) -> dispatch (OutputSelected (!! ev.target?value))) ] [
                                         for key, name in model.MIDIOutputs do
                                            yield option [ Key key ] [ str name ]
                                     ]
                        ]
                    ]
                    div [ ClassName "card-footer" ] [
                        button [ ClassName "btn btn-primary" 
                                 OnClick (fun _ -> dispatch SendNote) ] [ str "Send Note" ]
                    ]
                ]
            ]

            div [ ClassName "col" ] [
                div [ ClassName "card" ] [ 
                    div [ ClassName "card-header" ] [ strong [] [ str "MIDI Messages"] ]
                    div [ ClassName "card-body" ] [ 
                        for msg in model.Messages do
                            match msg with
                            | Info msg -> yield div [ ClassName "alert alert-info" ] [ str msg ]
                            | Success msg -> yield div [ ClassName "alert alert-success" ] [ str msg ]
                            | Warning msg -> yield div [ ClassName "alert alert-warning" ] [ str msg ]
                            | Error msg -> yield div [ ClassName "alert alert-danger" ] [ str msg ]
                    ]
                ]
            ]

            div [ClassName "col"] [
                div [ ClassName "chord" ] [
                    let notes = model.Notes |> Map.toList |> List.map fst
                    let root = Notes.getRoot notes
                    match root with
                    | Some root ->
                        let offset = Notes.offsetAgainstRoot root notes

                        yield sprintf "Root %A" (root |> Notes.toNote) |> str |> List.singleton |> div []
                        yield sprintf "Offsets %A" offset |> str |> List.singleton |> div [] 
                    | None -> ()
                ]
                div [ ClassName "notes" ] [
                    for n in model.Notes ->
                        div [ ClassName "card-body" ] [
                            sprintf "%A" n |> str
                            sprintf "Note %A" (n.Key |> Notes.toNote) |> str
                        ]
                ]
                div [ ClassName "sustained-notes" ] [
                    match model.NotesSustained with
                    | Some sustained ->
                        for n in sustained do
                            yield div [ ClassName "card-body" ] [
                                sprintf "%A" n |> str
                                sprintf "Sustained note %A" (n.Key |> Notes.toNote) |> str
                            ]
                    | None -> ()
                ]
            ]
        ]
    ]

open Elmish.React

Program.mkProgram init update view
|> Program.withReact "midi-app"
|> Program.run