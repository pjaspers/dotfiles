## Small CLI tools in lisp

My little toy directory with small CLI tools, all highly modeled after this setup: https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/

- `imdb-quote-fetcher` - Fetches all the quotes of a tt123456 number
- `css-color` - Can display the named css colors in your terminal
- `piek` - Control my tv via a harmony hub via home assistant
- `notify-ios` - Send a push notification to my phone form a CLI

## How to build

Just run `make` and it will the bin's that need building and the `man`-pages that need generating (nify trick is that since the `man` dir is next to a dir in the path `man css-color` will just work)

Now in order for make to just work:

- Install [sbcl](http://www.sbcl.org/platform-table.html) (brew install)
- Install [Quicklisp](https://www.quicklisp.org/beta/) (curl quicklisp, then `sbcl --load quicklisp.lisp`, then `(quicklisp-quickstart:install)`, then `(ql:add-to-init-file)`, then `(quit)`)

And that should be it.
