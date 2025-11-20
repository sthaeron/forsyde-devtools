#!/bin/bash

# Script for generating completions for bash/fish/zsh. 
# Notes:
# - Needs to be ran if arguments modules are changed!
# - Has to be ran from the root folder!!!

# For compiler:
forsyde-devtools-exe --bash-completion-script `which forsyde-devtools-exe` > ./.completions/forsyde-devtools-exe/bash
forsyde-devtools-exe --fish-completion-script `which forsyde-devtools-exe` > ./.completions/forsyde-devtools-exe/fish
forsyde-devtools-exe --zsh-completion-script `which forsyde-devtools-exe` > ./.completions/forsyde-devtools-exe/zsh

# For LSP:
forsyde-lsp-exe --bash-completion-script `which forsyde-lsp-exe` > ./.completions/forsyde-lsp-exe/bash
forsyde-lsp-exe --fish-completion-script `which forsyde-lsp-exe` > ./.completions/forsyde-lsp-exe/fish
forsyde-lsp-exe --zsh-completion-script `which forsyde-lsp-exe` > ./.completions/forsyde-lsp-exe/zsh