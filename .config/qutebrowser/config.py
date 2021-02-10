# qutemacs - a simple, preconfigured Emacs binding set for qutebrowser
#
# The aim of this binding set is not to provide bindings for absolutely
# everything, but to provide a stable launching point for people to make their
# own bindings.
#

# reload qutebrowser with C-x C-l (:config-source)

# set the color scheme
config.source('base16-qutebrowser/themes/default/base16-darkviolet.config.py')

config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103

c.tabs.background = True
# disable insert mode completely
c.input.insert_mode.auto_enter = False
c.input.insert_mode.auto_leave = False
c.input.insert_mode.plugins = False

# Forward unbound keys
c.input.forward_unbound_keys = "all"

ESC_BIND = 'clear-keychain ;; search ;; fullscreen --leave'

import string

c.bindings.default['normal'] = {}
c.bindings.default['insert'] = {}

c.bindings.commands['insert'] = {
    '<ctrl-space>': 'leave-mode',
    '<ctrl-g>': 'leave-mode;;fake-key <Left>;;fake-key <Right>',
    '<ctrl-f>': 'fake-key <Shift-Right>',
    '<ctrl-b>': 'fake-key <Shift-Left>',
    '<ctrl-e>': 'fake-key <Shift-End>',
    '<ctrl-a>': 'fake-key <Shift-Home>',
    '<ctrl-p>': 'fake-key <Shift-Up>',
    '<ctrl-n>': 'fake-key <Shift-Down>',
    '<Return>': 'leave-mode',
    '<ctrl-w>': 'fake-key <Ctrl-x>;;message-info "cut to clipboard";;leave-mode',
    '<alt-w>': 'fake-key <Ctrl-c>;;message-info "copy to clipboard";;leave-mode',
    '<ctrl-y>': 'fake-key <Ctrl-v>;;message-info "paste from clipboard";;leave-mode',
    '<backspace>': 'fake-key <backspace>;;leave-mode',
    '<alt-x>': 'leave-mode;;set-cmd-text :',
    '<alt-o>': 'leave-mode;;tab-focus last',
    '<Tab>': 'fake-key <f1>'
}



for char in list(string.ascii_lowercase):
    c.bindings.commands['insert'].update({char: 'fake-key ' + char + ';;leave-mode'})

for CHAR in list(string.ascii_uppercase):
    c.bindings.commands['insert'].update({CHAR: 'fake-key ' + char + ';;leave-mode'})

for num in list(map(lambda x : str(x), range(0, 10))):
    c.bindings.commands['insert'].update({num: 'fake-key ' + num + ';;leave-mode'})

for symb in [',', '.', '/', '\'', ';', '[', ']', '\\',
             '!', '@','#','$','%','^','&','*','(',')','-','_', '=', '+', '`', '~',
             ':', '\"', '<', '>', '?','{', '}', '|']:
    c.bindings.commands['insert'].update({symb: 'insert-text ' + symb + ' ;;leave-mode'})


# Bindings
c.bindings.commands['normal'] = {
    # Navigation
    '<ctrl-space>': 'enter-mode insert',
    '<ctrl-]>': 'fake-key <Ctrl-Shift-Right>',
    '<ctrl-[>': 'fake-key <Ctrl-Shift-Left>',
    '<ctrl-v>': 'scroll-page 0 0.5',
    '<alt-v>': 'scroll-page 0 -0.5',
    '<ctrl-shift-v>': 'scroll-page 0 1',
    '<alt-shift-v>': 'scroll-page 0 -1',

    '<alt-x>': 'set-cmd-text :',
    '<ctrl-x>b': 'set-cmd-text -s :tab-select;;fake-key <Down><Down><Down>',
    '<ctrl-x>k': 'tab-close',
    '<ctrl-x><ctrl-r>': 'config-cycle statusbar.hide',
    '<ctrl-x>1': 'tab-only;;message-info "cleared all other tabs"',
    '<ctrl-x><ctrl-c>': 'quit --save',

    # searching
    '<ctrl-s>': 'set-cmd-text /',
    '<ctrl-r>': 'set-cmd-text ?',

    # hinting
    '<alt-s>': 'hint all',

    # tabs
    '<ctrl-tab>': 'tab-next',
    '<ctrl-shift-tab>': 'tab-prev',

    # open links
    '<ctrl-l>': 'set-cmd-text -s :open',
    '<alt-l>': 'set-cmd-text -s :open -t',

    # reload page
    '<ctrl-x>r': 'reload',


    # history
    '<alt-h>': 'history -t',

    # editing
    '<alt-[>': 'back',
    '<alt-]>': 'forward',
    '<ctrl-/>': 'fake-key <Ctrl-z>',
    '<ctrl-shift-?>': 'fake-key <Ctrl-Shift-z>',
    '<ctrl-k>': 'fake-key <Shift-End>;;fake-key <Backspace>',
    '<ctrl-f>': 'fake-key <Right>',
    '<ctrl-b>': 'fake-key <Left>',
    '<alt-o>': 'tab-focus last',
    '<ctrl-a>': 'fake-key <Home>',
    '<ctrl-x>h': 'fake-key <Ctrl-a>',
    '<ctrl-e>': 'fake-key <End>',
    '<ctrl-n>': 'fake-key <Down>',
    '<ctrl-p>': 'fake-key <Up>',
    '<alt-f>': 'fake-key <Ctrl-Right>',
    '<alt-b>': 'fake-key <Ctrl-Left>',
    '<ctrl-d>': 'fake-key <Delete>',
    '<alt-d>': 'fake-key <Ctrl-Delete>',
    '<alt-backspace>': 'fake-key <Ctrl-Backspace>',
    '<ctrl-w>': 'fake-key <Ctrl-x>;;message-info "cut to clipboard"',
    '<alt-w>': 'fake-key <Ctrl-c>;;message-info "copy to clipboard"',
#    '<ctrl-y>': 'insert-text {primary}',
    '<ctrl-y>': 'fake-key <Ctrl-v>;;message-info "paste from clipbaord"',

    '1': 'fake-key 1',
    '2': 'fake-key 2',
    '3': 'fake-key 3',
    '4': 'fake-key 4',
    '5': 'fake-key 5',
    '6': 'fake-key 6',
    '7': 'fake-key 7',
    '8': 'fake-key 8',
    '9': 'fake-key 9',
    '0': 'fake-key 0',

    # zoom
    '<ctrl-x>+': 'zoom-in',
    '<ctrl-x>-': 'zoom-out',
    '<ctrl-x>=': 'zoom',

    # escape hatch
    '<ctrl-h>': 'set-cmd-text -s :help',
    '<ctrl-g>': ESC_BIND,
}

c.bindings.commands['command'] = {
    '<ctrl-s>': 'search-next',
    '<ctrl-r>': 'search-prev',

    '<ctrl-p>': 'completion-item-focus prev',
    '<ctrl-n>': 'completion-item-focus next',

    '<alt-p>': 'command-history-prev',
    '<alt-n>': 'command-history-next',

	# escape hatch
    '<ctrl-g>': 'leave-mode',
}

c.bindings.commands['hint'] = {
    # escape hatch
    '<ctrl-g>': 'leave-mode',
}


c.bindings.commands['caret'] = {
    # escape hatch
    '<ctrl-g>': 'leave-mode',
    '<ctrl-space>': 'toggle-selection'
}

config.bind('<Tab>', 'fake-key <f1>')
config.bind('<Ctrl-x><Ctrl-l>', 'config-source')
c.tabs.show = 'never'
c.statusbar.show = 'always'
c.url.searchengines["g"] = "https://www.google.com/search?q={}"
c.url.searchengines["DEFAULT"] = "https://www.google.com/search?q={}"
c.auto_save.session = True # always save sessions
c.tabs.background = True   # open new tabs in the background with Ctrl + click
c.content.autoplay = False # don't play videos automatically


# load autoconfig - useful for getting permissions choices loaded (eg permissions to notify)
config.load_autoconfig()
