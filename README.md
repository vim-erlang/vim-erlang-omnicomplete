# vim-erlang-omnicomplete

`vim-erlang-omnicomplete` is an Erlang **autocompletion plugin** for Vim.

## Installation

With [pathogen.vim](https://github.com/tpope/vim-pathogen):

1.  `cd ~/.vim/bundle` and clone this repository.

2.  Generate help page:

    ```
    :Helptags
    ```

Manually:

1.  Clone this repository.

2.  Add the following line to your `.vimrc` (replace the path with your own):

    ```
    :set runtimepath^=/path/to/vim-erlang-omnicomplete
    ```

3.  Restart Vim (or execute the command above).

4.  Generate help page:

    ```
    :helptags /path/to/vim-erlang-omnicomplete/doc
    ```

## Documentation

- On the web: [user documentation][doc].
- Inside Vim: `:help vim-erlang-omnicomplete`.

## Contributing

*   Please read the [Contributing][vim-erlang-contributing] section of the
    vim-erlang README.

*   If you modify `erlang_complete.erl`, please update the tests in in the
    vim-erlang repository.

[doc]: https://github.com/vim-erlang/vim-erlang-omnicomplete/blob/master/doc/vim-erlang-omnicomplete.txt
[vim-erlang-contributing]: https://github.com/vim-erlang/vim-erlang#contributing
