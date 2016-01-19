# Emacs Party Mode

I could describe it, but why not just give it a try instead? Don't tell me you don't like to party.

## Pre-reqs

You must have installed and configured the Emacs Multimedia System ([EMMS](https://www.gnu.org/software/emms/)) for playing mp3s.

Oh also you need [emacs-async](https://github.com/jwiegley/emacs-async).

Both of the above can be installed via `M-x list-packages`.

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

## Shouts outs

Thanks a million to [CRAPFACE](https://soundcloud.com/crapface) for letting me use a track from [this awesome mix](https://soundcloud.com/1833-fm/1833-mix-series-vol-78-crapface).

:sparkling_heart: :two hearts: :notes:

## License

MIT
