# Emacs Party Mode

I could describe it, but why not just give it a try instead? Don't tell me you don't like to party.

## Pre-reqs

You must have installed and configured the Emacs multimedia system (EMMS) for playing mp3s.

## Installation

Clone this repository and add the directory to your load path, like this:

`$ git clone git@github.com:parkeristyping/party_mode.git`

Then add this to your `.emacs` or `init.el` file:

``` lisp
(add-to-list 'load-path "~/path/to/party_mode")
(load "party_mode.el")
```

## Usage

Party with `M-x party-mode RET`

Go back to work with `M-x work-mode RET`

## License

MIT
