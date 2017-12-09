# Fable WebMIDI Sample

This sample contains a simple web app with [Fable] and a [Web MIDI] implementation with [Fable].

This is the complementary sample for my [F# Advent 2017] blog post at http://blog.pagansoft.de/articles/a-fable-of-webmidi/

## Requirements

* [dotnet SDK] 2.0 or higher
* [node.js] 6.11 or higher
* A JS package manager: [yarn] or [npm]

> npm comes bundled with node.js, but we recommend to use at least npm 5. If you have npm installed, you can upgrade it by running `npm install -g npm`.

Although is not a Fable requirement, on macOS and Linux you'll need [Mono] for other F# tooling like Paket or editor support.

## Editor

The project can be used by editors compatible with the new .fsproj format, like VS Code + [Ionide], Emacs with [fsharp-mode] or [Rider]. **Visual Studio for Mac** is also compatible but in the current version the package auto-restore function conflicts with Paket so you need to disable it: `Preferences > Nuget > General`.

## Building and running the app

> In the commands below, yarn is the tool of choice. If you want to use npm, just replace `yarn` by `npm` in the commands.

* Install JS dependencies: `yarn install`
* **Move to `src` folder**: `cd src`
* Install F# dependencies: `dotnet restore`
* Start Fable daemon and [Webpack] dev server: `dotnet fable yarn-start`
* In your browser, open: http://localhost:8080/

> `dotnet fable yarn-start` (or `npm-start`) is used to start the Fable daemon and run a script in package.json concurrently. It's a shortcut of `yarn-run [SCRIPT_NAME]`, e.g. `dotnet fable yarn-run start`.

If you are using VS Code + [Ionide], you can also use the key combination: Ctrl+Shift+B (Cmd+Shift+B on macOS) instead of typing the `dotnet fable yarn-start` command. This also has the advantage that Fable-specific errors will be highlighted in the editor along with other F# errors.

Any modification you do to the F# code will be reflected in the web page after saving. When you want to output the JS code to disk, run `dotnet fable yarn-build` and you'll get a minified JS bundle in the `public` folder.

## JS Output

This template uses [babel-preset-env] to output JS code whose syntax is compatible with a wide range of browsers. Currently it's set to support only Chrome.

## Project structure

### Paket

[Paket] is the package manager used for F# dependencies. It doesn't need a global installation, the binary is included in the `.paket` folder. Other Paket related files are:

- **paket.dependencies**: contains all the dependencies in the repository.
- **paket.references**: there should be one such a file next to each `.fsproj` file.
- **paket.lock**: automatically generated, but should be committed to source control, [see why](https://fsprojects.github.io/Paket/faq.html#Why-should-I-commit-the-lock-file).
- **Nuget.Config**: prevents conflicts with Paket in machines with some Nuget configuration.

> Paket dependencies will be installed in the `packages` directory. See [Paket website] for more info.

### yarn/npm

- **package.json**: contains the JS dependencies together with other info, like development scripts.
- **yarn.lock**: is the lock file created by yarn.
- **package-lock.json**: is the lock file understood by npm 5, if you use it instead of yarn.

> JS dependencies will be installed in `node_modules`. See [yarn] and/or [npm] websites for more info.

### Webpack

[Webpack] is a bundler, which links different JS sources into a single file making deployment much easier. It also offers other features, like a static dev server that can automatically refresh the browser upon changes in your code or a minifier for production release. Fable interacts with Webpack through the `fable-loader`.

- **webpack.config.js**: is the configuration file for Webpack. It allows you to set many things: like the path of the bundle, the port for the development server or [Babel] options. See [Webpack] website for more info.

### F# source files

The template only contains three F# source files: the project (.fsproj), the WebMIDI module (`WebMIDI.fs`) and the Application source file (`App.fs`) in `src` folder.

## Where to go from here

Check more [Fable samples](https://github.com/fable-compiler/samples-browser) and check the [awesome-fable](https://github.com/kunjee17/awesome-fable#-awesome-fable) for a curated list of resources provided by the community.


[Fable]: http://fable.io/
[Web MIDI]: https://www.w3.org/TR/webmidi/
[dotnet SDK]: https://www.microsoft.com/net/download/core
[node.js]: https://nodejs.org
[yarn]: https://yarnpkg.com
[npm]: http://npmjs.com/
[Mono]: http://www.mono-project.com/
[Ionide]: http://ionide.io/
[fsharp-mode]: https://github.com/fsharp/emacs-fsharp-mode
[Rider]: https://www.jetbrains.com/rider/
[Webpack]: https://webpack.js.org/
[babel-preset-env]: http://babeljs.io/env
[Paket]: https://fsprojects.github.io/Paket/
[Paket website]: https://fsprojects.github.io/Paket/
[Babel]: https://babeljs.io/
[F# Adevent 2017]: https://sergeytihon.com/2017/10/22/f-advent-calendar-in-english-2017/
