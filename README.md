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

## Quick start

1.  Open an Erlang source file.

2.  Start typing something (e.g. `li` or `io:fo`).

3.  Without leaving insert mode, hit CTRL-X and then CTRL-O.

4.  After at most a few seconds, you should see a list of completions.

## Documentation

*   Vim's omni completion (i.e., autocomplete) functionality:
    [`:help compl-omni`].

*   Vim's `completeopt` option to configure omni completion
    [`:help completeopt`].

*   `vim-erlang-omnicomplete` plugin: [`:help vim-erlang-omnicomplete`].

## Development

### File layout

This repository contains the following files and directories:

<!-- If you edit the list, please keep the alphabetical order. -->

*   [`autoload/erlang_complete.erl`]: Erlang script which can analyse the code
    base and calculate the list of modules and the list of functions in
    a module.

*   [`autoload/erlang_complete.vim`]: This script contains most of
    `vim-erlang-omnicomplete`'s functionality on the Vim side.

    This functionality is here (instead of [`plugin/erlang_omnicomplete.vim`])
    so that Vim's autoload functionality ([`:help autoload`]) can make sure that
    this script is executed only when needed the first time.

*   [`doc/vim-erlang-omnicomplete.txt`]: This file contains the user
    documentation of `vim-erlang-omnicomplete`.

*   [`ftplugin/erlang.vim`]: This script sets up `vim-erlang-omnicomplete` when
    an Erlang source file is opened.

*   [`plugin/erlang_omnicomplete.vim`]: This script sets up
    `vim-erlang-omnicomplete` when Vim is started. It is kept small so that the
    effect on Vim's startup time is minimal.

## Contributing

*   Please read the [Contributing][vim-erlang-contributing] section of the
    vim-erlang README.

*   If you modify [`autoload/erlang_complete.erl`], please update the tests in
    in the vim-erlang repository.

[`:help autoload`]: https://vimhelp.org/eval.txt.html#autoload
[`:help compl-omni`]: https://vimhelp.org/insert.txt.html#compl-omni
[`:help completeopt`]: https://vimhelp.org/options.txt.html#%27completeopt%27
[`:help packages`]: https://vimhelp.org/repeat.txt.html#packages
[`:help vim-erlang-omnicomplete`]: doc/vim-erlang-omnicomplete.txt
[`autoload/erlang_complete.erl`]: autoload/erlang_complete.erl
[`autoload/erlang_complete.vim`]: autoload/erlang_complete.vim
[`doc/vim-erlang-omnicomplete.txt`]: doc/vim-erlang-omnicomplete.txt
[`ftplugin/erlang.vim`]: ftplugin/erlang.vim
[`plugin/erlang_omnicomplete.vim`]: plugin/erlang_omnicomplete.vim
[vim-erlang-contributing]: https://github.com/vim-erlang/vim-erlang#contributing
