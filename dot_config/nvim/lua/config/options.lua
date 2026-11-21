-- CUSTOM GLOBALS
-- Leader Keys
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Orgmode
vim.g.org_directory = vim.fn.expand("~/Library/CloudStorage/ProtonDrive-borangoher@proton.me-folder/org/")

-- LAZYVIM GLOBALS
-- LazyVim Base
vim.g.autoformat = true
vim.g.snacks_animate = true
vim.g.lazyvim_picker = "fzf"
vim.g.lazyvim_cmp = "blink.cmp"
vim.g.ai_cmp = true

-- LSP
vim.g.root_spec = { "lsp", { ".git", "lua" }, "cwd" }
vim.g.root_lsp_ignore = { "copilot" }

-- Language Specific
vim.g.markdown_recommended_style = 0
vim.g.loaded_perl_provider = 0
vim.g.lazyvim_python_lsp = "pyright"
vim.g.lazyvim_python_ruff = "ruff"

-- Other
vim.g.deprecation_warnings = false
vim.g.trouble_lualine = true

-- VIM OPTIONS
local opt = vim.opt

-- basics
opt.spelllang = { "en" }
opt.autowrite = true
opt.clipboard = vim.env.SSH_CONNECTION and "" or "unnamedplus" -- Sync with system clipboard
opt.completeopt = "menu,menuone,noselect"
opt.conceallevel = 2 -- Hide * markup for bold and italic, but not markers with substitutions
opt.confirm = true
opt.cursorline = true
opt.updatetime = 300
opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode
opt.smartcase = true -- Don't ignore case with capitals
opt.termguicolors = true -- True color support
opt.timeoutlen = vim.g.vscode and 1000 or 300 -- Lower than default (1000) to quickly trigger which-key
opt.winminwidth = 5 -- Minimum window width
opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize", "help", "globals", "skiprtp", "folds" }
opt.shortmess:append({ W = true, I = true, c = true, C = true })

-- backup/swap etc.
opt.swapfile = false
opt.backup = false
opt.writebackup = false

-- undo
opt.undofile = true
opt.undolevels = 10000
opt.undoreload = 10000

-- cmdline
opt.wildmode = "longest:full,full"

-- statusline
opt.laststatus = 3
opt.statuscolumn = [[%!v:lua.LazyVim.statuscolumn()]]

opt.signcolumn = "number"

-- tabs and spaces
opt.expandtab = false
opt.tabStop = 2
opt.shiftWidth = 2
opt.softtabstop = 2
opt.tabstop = 2 -- Number of spaces tabs count for

-- line numbers
opt.number = true
opt.relativenumber = true

-- mouse
opt.mouse = "a" -- Enable mouse mode

-- fold settings
opt.foldlevel = 99
opt.foldmethod = "indent"
opt.foldtext = ""

-- indent
opt.shiftround = true -- Round indent
opt.shiftwidth = 2 -- Size of an indent
opt.smartindent = true -- Insert indents automatically
opt.wrap = false -- Disable line wrap
opt.inccommand = "nosplit" -- preview incremental substitute
opt.jumpoptions = "view"
opt.ruler = false -- Disable the default ruler

-- format
opt.formatexpr = "v:lua.LazyVim.format.formatexpr()"
opt.formatoptions = "jcroqlnt" -- tcqj

-- grep
opt.grepformat = "%f:%l:%c:%m"
opt.grepprg = "rg --vimgrep"
opt.ignorecase = true -- Ignore case

-- split
opt.splitbelow = true -- Put new windows below current
opt.splitkeep = "screen"
opt.splitright = true -- Put new windows right of current

-- popup
opt.pumblend = 10 -- Popup blend
opt.pumheight = 10 -- Maximum number of entries in a popup

-- Movement
opt.whichwrap = "" -- Don't allow automatic wrap around (by backspace for example)
opt.startofline = false -- Don't move to start of line on jump

-- Search
opt.wrapscan = true -- Search commands (including spellcheck etc.) wrap around
opt.incsearch = true -- Show match for partly typed search command
opt.ignorecase = true -- Ignore case by default for searches
opt.smartcase = true -- Override ignore case for searches with upper case characters
opt.casemap = { "internal", "keepascii" } -- keepascii is required to handle pairs like i-I independent of locale (Turkish being the possible culprit)

-- Scrolling
opt.scrolloff = 8 -- Vertical offset
opt.sidescrolloff = 8 -- Horizontal offset
opt.smoothscroll = true
opt.scroll = 0 -- Scroll length for <C-U>, <C-D>. 0 is for half of window size.

-- Wrapping
opt.wrap = true
opt.linebreak = true -- Wrapping preserves words etc.
opt.breakindent = true -- Preserve indent level after wrap
opt.showbreak = "> " -- String to display at the beginning of wrapped lines

-- Display
opt.fillchars = {
	stl = " ",
	stlnc = " ",
	vert = "|",
	fold = " ",
	foldopen = "",
	foldclose = "",
	foldsep = " ",
	diff = "╱",
	eob = " ",
	lastline = "@",
	trunc = ">",
	truncrl = "<",
	tpl_vert = "|",
}
opt.cmdheight = 0
opt.list = true -- Show invisible chars in insert mode

--
--  4 displaying text
-- display	include "lastline" to show the last line even if it doesn't fit
-- 	include "uhex" to show unprintable characters as a hex number
--  	set dy=lastline
-- fillchars	characters to use for the status line, folds, diffs,
-- 	buffer text, filler lines and truncation in the completion menu
-- 	(global or local to window)
--  	set fcs=diff:╱,eob: ,fold: ,foldclose:,foldopen:,foldsep:
-- cmdheight	number of lines used for the command-line
--  	set ch=0
-- columns	width of the display
--  	set co=229
-- lines	number of lines in the display
--  	set lines=65
-- window	number of lines to scroll for CTRL-F and CTRL-B
--  	set wi=64
-- lazyredraw	don't redraw while executing macros
--  	set nolz	lz
-- redrawtime	timeout for 'hlsearch' and :match highlighting in msec
--  	set rdt=2000
-- writedelay	delay in msec for each char written to the display
--  	set wd=0
-- redrawdebug	change the way redrawing works (debug)
--  	set rdb=
-- list	show <Tab> as ^I and end-of-line as $
-- 	(local to window)
--  	set list	nolist
-- listchars	list of strings used for list mode
-- 	(global or local to window)
--  	set lcs=tab:> ,trail:-,nbsp:+
-- number	show the line number for each line
-- 	(local to window)
--  	set nu	nonu
-- relativenumber	show the relative line number for each line
-- 	(local to window)
--  	set rnu	nornu
-- numberwidth	number of columns to use for the line number
-- 	(local to window)
--  	set nuw=4
-- chistory	maximum number of quickfix lists that can be stored in history
--  	set chi=10
-- lhistory	maximum number of location lists that can be stored in history
-- 	(local to window)
--  	set lhi=10
-- conceallevel	controls whether concealable text is hidden
-- 	(local to window)
--  	set cole=2
-- concealcursor	modes in which text in the cursor line can be concealed
-- 	(local to window)
--  	set cocu=
--
--  5 syntax, highlighting and spelling
--
-- background	"dark" or "light"; the background color brightness
--  	set bg=dark
-- filetype	type of file; triggers the FileType event when set
-- 	(local to buffer)
--  	set ft=lua
-- syntax	name of syntax highlighting used
-- 	(local to buffer)
--  	set syn=
-- synmaxcol	maximum column to look for syntax items
-- 	(local to buffer)
--  	set smc=3000
-- hlsearch	highlight all matches for the last used search pattern
--  	set hls	nohls
-- termguicolors	use GUI colors for the terminal
--  	set tgc	notgc
-- cursorcolumn	highlight the screen column of the cursor
-- 	(local to window)
--  	set nocuc	cuc
-- cursorline	highlight the screen line of the cursor
-- 	(local to window)
--  	set cul	nocul
-- cursorlineopt	specifies which area 'cursorline' highlights
-- 	(local to window)
--  	set culopt=both
-- colorcolumn	columns to highlight
-- 	(local to window)
--  	set cc=
-- spell	highlight spelling mistakes
-- 	(local to window)
--  	set nospell	spell
-- spelllang	list of accepted languages
-- 	(local to buffer)
--  	set spl=en
-- spellfile	file that "zg" adds good words to
-- 	(local to buffer)
--  	set spf=
-- spellcapcheck	pattern to locate the end of a sentence
-- 	(local to buffer)
--  	set spc=[.?!]\_[\])'"\t ]\+
-- spelloptions	flags to change how spell checking works
-- 	(local to buffer)
--  	set spo=noplainbuffer
-- spellsuggest	methods used to suggest corrections
--  	set sps=best
-- mkspellmem	amount of memory used by :mkspell before compressing
--  	set msm=460000,2000,500
-- winhighlight	override highlighting-groups window-locally
-- 	(local to window)
--  	set winhl=
--
--  6 multiple windows
--
-- laststatus	0, 1, 2 or 3; when to use a status line for the last window
--  	set ls=3
-- statuscolumn	custom format for the status column
-- 	(local to window)
--  	set stc=%!v:lua.LazyVim.statuscolumn()
-- statusline	alternate format to be used for a status line
-- 	(global or local to window)
--  	set stl=%#lualine_a_command# COMMAND %#lualine_transitional_lualine_a_command_to_lualine_b_command#%#lualine_b_command#  master %#lualine_transitional_lualine_b_command_to_lualine_c_filetype_MiniIconsAzure_command#%<%#lualine_c_filetype_MiniIconsAzure_command# 󰢱 %#lualine_c_command#%#lualine_c_command#%#lualine_c_command# dot_config/…/config/%#lualine_c_9_LV_Bold_command#options.lua%#lualine_c_command# %#lualine_c_command#%=%#lualine_x_13_command# : %#lualine_c_command#%#lualine_x_diff_added_command#  72 %#lualine_x_diff_modified_command# 22 %#lualine_x_diff_removed_command# 1 %#lualine_transitional_lualine_b_command_to_lualine_x_diff_removed_command#%#lualine_b_command# 42%% %#lualine_b_command# 46:18 %#lualine_transitional_lualine_a_command_to_lualine_b_command#%#lualine_a_command#  16:07
-- equalalways	make all windows the same size when adding/removing windows
--  	set ea	noea
-- eadirection	in which direction 'equalalways' works: "ver", "hor" or "both"
--  	set ead=both
-- winheight	minimal number of lines used for the current window
--  	set wh=1
-- winminheight	minimal number of lines used for any window
--  	set wmh=1
-- winfixbuf	keep window focused on a single buffer
-- 	(local to window)
--  	set nowfb	wfb
-- winfixheight	keep the height of the window
-- 	(local to window)
--  	set nowfh	wfh
-- winfixwidth	keep the width of the window
-- 	(local to window)
--  	set nowfw	wfw
-- winwidth	minimal number of columns used for the current window
--  	set wiw=20
-- winminwidth	minimal number of columns used for any window
--  	set wmw=5
-- helpheight	initial height of the help window
--  	set hh=20
-- previewheight	default height for the preview window
--  	set pvh=12
-- previewwindow	identifies the preview window
-- 	(local to window)
--  	set nopvw	pvw
-- winbar	custom format for the window bar
-- 	(global or local to window)
--  	set wbr=
-- winborder	border of floating window
--  	set winborder=
-- winblend	transparency level for floating windows
-- 	(local to window)
--  	set winbl=0
-- hidden	don't unload a buffer when no longer shown in a window
--  	set hid	nohid
-- switchbuf	"useopen" and/or "split"; which window to use when jumping
-- 	to a buffer
--  	set swb=uselast
-- splitbelow	a new window is put below the current one
--  	set sb	nosb
-- splitkeep	determines scroll behavior for split windows
--  	set spk=screen
-- splitright	a new window is put right of the current one
--  	set spr	nospr
-- scrollbind	this window scrolls together with other bound windows
-- 	(local to window)
--  	set noscb	scb
-- scrollopt	"ver", "hor" and/or "jump"; list of options for 'scrollbind'
--  	set sbo=ver,jump
-- cursorbind	this window's cursor moves together with other bound windows
-- 	(local to window)
--  	set nocrb	crb
--
--  7 multiple tab pages
--
-- showtabline	0, 1 or 2; when to use a tab pages line
--  	set stal=2
-- tabclose	behaviour when closing tab pages: left, uselast or empty
--  	set tcl=
-- tabpagemax	maximum number of tab pages to open for -p and "tab all"
--  	set tpm=50
-- tabline	custom tab pages line
--  	set tal=%!v:lua.nvim_bufferline()
--
--  8 terminal
--
-- scrolljump	minimal number of lines to scroll at a time
--  	set sj=1
-- guicursor	specifies what the cursor looks like in different modes
--  	set gcr=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20,t:block-blinkon500-blinkoff500-TermCursor
-- title	show info in the window title
--  	set notitle	title
-- titlelen	percentage of 'columns' used for the window title
--  	set titlelen=85
-- titlestring	when not empty, string to be used for the window title
--  	set titlestring=
-- titleold	string to restore the title to when exiting Vim
--  	set titleold=
-- icon	set the text of the icon for this window
--  	set noicon	icon
-- iconstring	when not empty, text for the icon of this window
--  	set iconstring=
--
--  9 using the mouse
--
-- mouse	list of flags for using the mouse
--  	set mouse=a
-- mousescroll	amount to scroll by when scrolling with a mouse
--  	set mousescroll=ver:3,hor:6
-- mousefocus	the window with the mouse pointer becomes the current one
--  	set nomousef	mousef
-- mousehide	hide the mouse pointer while typing
--  	set mh	nomh
-- mousemodel	"extend", "popup" or "popup_setpos"; what the right
-- 	mouse button is used for
--  	set mousem=popup_setpos
-- mousetime	maximum time in msec to recognize a double-click
--  	set mouset=500
-- mousemoveevent	deliver mouse move events to input queue
--  	set nomousemev	mousemev
--
-- 10 GUI
--
-- guifont	list of font names to be used in the GUI
--  	set gfn=SF Mono,Menlo,Monaco,Courier New,monospace
-- guifontwide	list of font names to be used for double-wide characters
--  	set gfw=
-- browsedir	"last", "buffer" or "current": which directory used for
-- 	the file browser
--  	set bsdir=
-- langmenu	language to be used for the menus
--  	set lm=
-- menuitems	maximum number of items in one menu
--  	set mis=25
-- winaltkeys	"no", "yes" or "menu"; how to use the ALT key
--  	set wak=menu
-- linespace	number of pixel lines to use between characters
--  	set lsp=0
-- termsync	synchronize redraw output with the host terminal
--  	set termsync	notermsync
--
-- 11 messages and info
--
-- shortmess	list of flags to make messages shorter
--  	set shm=ClFoOtTWcI
-- messagesopt	options for outputting messages
--  	set mopt=hit-enter,history:500,progress:c
-- showcmd	show (partial) command keys in location given by 'showcmdloc'
--  	set sc	nosc
-- showcmdloc	location where to show the (partial) command keys for 'showcmd'
--  	set sloc=last
-- showmode	display the current mode in the status line
--  	set nosmd	smd
-- ruler	show cursor position below each window
--  	set noru	ru
-- rulerformat	alternate format to be used for the ruler
--  	set ruf=
-- report	threshold for reporting number of changed lines
--  	set report=2
-- verbose	the higher the more messages are given
--  	set vbs=0
-- verbosefile	file to write messages in
--  	set vfile=
-- more	pause listings when the screen is full
--  	set more	nomore
-- confirm	start a dialog when a command fails
--  	set cf	nocf
-- errorbells	ring the bell for error messages
--  	set noeb	eb
-- visualbell	use a visual bell instead of beeping
--  	set novb	vb
-- belloff	do not ring the bell for these reasons
--  	set bo=all
-- helplang	list of preferred languages for finding help
--  	set hlg=en
--
-- 12 selecting text
--
-- selection	"old", "inclusive" or "exclusive"; how selecting text behaves
--  	set sel=inclusive
-- selectmode	"mouse", "key" and/or "cmd"; when to start Select mode
-- 	instead of Visual mode
--  	set slm=
-- clipboard	"unnamed" to use the * register like unnamed register
-- 	"autoselect" to always put selected text on the clipboard
--  	set cb=unnamedplus
-- keymodel	"startsel" and/or "stopsel"; what special keys can do
--  	set km=
--
-- 13 editing text
--
-- undolevels	maximum number of changes that can be undone
-- 	(global or local to buffer)
--  	set ul=10000
-- undofile	automatically save and restore undo history
-- 	(local to buffer)
--  	set udf	noudf
-- undodir	list of directories for undo files
--  	set udir=/Users/boran/.local/state/nvim/undo//
-- undoreload	maximum number lines to save for undo on a buffer reload
--  	set ur=10000
-- modified	changes have been made and not written to a file
-- 	(local to buffer)
--  	set nomod	mod
-- readonly	buffer is not to be written
-- 	(local to buffer)
--  	set noro	ro
-- modifiable	changes to the text are possible
-- 	(local to buffer)
--  	set ma	noma
-- textwidth	line length above which to break a line
-- 	(local to buffer)
--  	set tw=0
-- wrapmargin	margin from the right in which to break a line
-- 	(local to buffer)
--  	set wm=0
-- backspace	specifies what <BS>, CTRL-W, etc. can do in Insert mode
--  	set bs=indent,eol,start
-- comments	definition of what comment lines look like
-- 	(local to buffer)
--  	set com=:---,:--
-- commentstring	template for comments; used to put the marker in
-- 	(local to buffer)
--  	set cms=-- %s
-- formatoptions	list of flags that tell how automatic formatting works
-- 	(local to buffer)
--  	set fo=nlqrjco
-- formatlistpat	pattern to recognize a numbered list
-- 	(local to buffer)
--  	set flp=^\s*\d\+[\]:.)}\t ]\s*
-- formatexpr	expression used for "gq" to format lines
-- 	(local to buffer)
--  	set fex=v:lua.LazyVim.format.formatexpr()
-- complete	specifies how Insert mode completion works for CTRL-N and CTRL-P
-- 	(local to buffer)
--  	set cpt=.,w,b,u,t
-- autocomplete	automatic completion in insert mode
-- 	(global or local to buffer)
--  	set noac	ac
-- autocompletetimeout
-- 	initial decay timeout for 'autocomplete' algorithm
--  	set act=80
-- completetimeout
-- 	initial decay timeout for CTRL-N and CTRL-P completion
--  	set cto=0
-- autocompletedelay
-- 	delay in msec before menu appears after typing
--  	set acl=0
-- completeopt	whether to use a popup menu for Insert mode completion
-- 	(global or local to buffer)
--  	set cot=menu,menuone,noselect
-- completeitemalign
-- 	popup menu item align order
--  	set cia=abbr,kind,menu
-- pumheight	maximum height of the popup menu
--  	set ph=10
-- pumwidth	minimum width of the popup menu
--  	set pw=15
-- pummaxwidth	maximum width of the popup menu
--  	set pmw=0
-- pumblend	transparency level of popup menu
--  	set pb=10
-- pumborder	border of popupmenu
--  	set pumborder=
-- completefunc	user defined function for Insert mode completion
-- 	(local to buffer)
--  	set cfu=
-- omnifunc	function for filetype-specific Insert mode completion
-- 	(local to buffer)
--  	set ofu=v:lua.vim.lsp.omnifunc
-- dictionary	list of dictionary files for keyword completion
-- 	(global or local to buffer)
--  	set dict=
-- thesaurus	list of thesaurus files for keyword completion
-- 	(global or local to buffer)
--  	set tsr=
-- thesaurusfunc	function used for thesaurus completion
-- 	(global or local to buffer)
--  	set tsrfu=
-- infercase	adjust case of a keyword completion match
-- 	(local to buffer)
--  	set inf	noinf
-- digraph	enable entering digraphs with c1 <BS> c2
--  	set nodg	dg
-- tildeop	the "~" command behaves like an operator
--  	set notop	top
-- operatorfunc	function called for the "g@" operator
--  	set opfunc=
-- showmatch	when inserting a bracket, briefly jump to its match
--  	set nosm	sm
-- matchtime	tenth of a second to show a match for 'showmatch'
--  	set mat=5
-- matchpairs	list of pairs that match for the "%" command
-- 	(local to buffer)
--  	set mps=(:),{:},[:]
-- joinspaces	use two spaces after '.' when joining a line
--  	set nojs	js
-- nrformats	"alpha", "octal", "hex", "bin" and/or "unsigned"; number formats
-- 	recognized for CTRL-A and CTRL-X commands
-- 	(local to buffer)
--  	set nf=bin,hex
--
-- 14 tabs and indenting
--
-- tabstop	number of spaces a <Tab> in the text stands for
-- 	(local to buffer)
--  	set ts=2
-- shiftwidth	number of spaces used for each step of (auto)indent
-- 	(local to buffer)
--  	set sw=2
-- vartabstop	list of number of spaces a tab counts for
-- 	(local to buffer)
--  	set vts=
-- varsofttabstop	list of number of spaces a soft tabsstop counts for
-- 	(local to buffer)
--  	set vsts=
-- smarttab	a <Tab> in an indent inserts 'shiftwidth' spaces
--  	set sta	nosta
-- softtabstop	if non-zero, number of spaces to insert for a <Tab>
-- 	(local to buffer)
--  	set sts=2
-- shiftround	round to 'shiftwidth' for "<<" and ">>"
--  	set sr	nosr
-- expandtab	expand <Tab> to spaces in Insert mode
-- 	(local to buffer)
--  	set et	noet
-- autoindent	automatically set the indent of a new line
-- 	(local to buffer)
--  	set ai	noai
-- smartindent	do clever autoindenting
-- 	(local to buffer)
--  	set si	nosi
-- cindent	enable specific indenting for C code
-- 	(local to buffer)
--  	set nocin	cin
-- cinoptions	options for C-indenting
-- 	(local to buffer)
--  	set cino=
-- cinkeys	keys that trigger C-indenting in Insert mode
-- 	(local to buffer)
--  	set cink=0{,0},0),0],:,0#,!^F,o,O,e
-- cinwords	list of words that cause more C-indent
-- 	(local to buffer)
--  	set cinw=if,else,while,do,for,switch
-- cinscopedecls	list of scope declaration names used by cino-g
-- 	(local to buffer)
--  	set cinsd=public,protected,private
-- indentexpr	expression used to obtain the indent of a line
-- 	(local to buffer)
--  	set inde=v:lua.LazyVim.treesitter.indentexpr()
-- indentkeys	keys that trigger indenting with 'indentexpr' in Insert mode
-- 	(local to buffer)
--  	set indk=0{,0},0),0],:,0#,!^F,o,O,e,0=end,0=until
-- copyindent	copy whitespace for indenting from previous line
-- 	(local to buffer)
--  	set noci	ci
-- preserveindent	preserve kind of whitespace when changing indent
-- 	(local to buffer)
--  	set nopi	pi
-- lisp	enable lisp mode
-- 	(local to buffer)
--  	set nolisp	lisp
-- lispwords	words that change how lisp indenting works
-- 	(global or local to buffer)
--  	set lw=defun,define,defmacro,set!,lambda,if,case,let,flet,let*,letrec,do,do*,define-syntax,let-syntax,letrec-syntax,destructuring-bind,defpackage,defparameter,defstruct,deftype,defvar,do-all-symbols,do-external-symbols,do-symbols,dolist,dotimes,ecase,etypecase,eval-when,labels,macrolet,multiple-value-bind,multiple-value-call,multiple-value-prog1,multiple-value-setq,prog1,progv,typecase,unless,unwind-protect,when,with-input-from-string,with-open-file,with-open-stream,with-output-to-string,with-package-iterator,define-condition,handler-bind,handler-case,restart-bind,restart-case,with-simple-restart,store-value,use-value,muffle-warning,abort,continue,with-slots,with-slots*,with-accessors,with-accessors*,defclass,defmethod,print-unreadable-object
-- lispoptions	options for Lisp indenting
-- 	(local to buffer)
--  	set lop=
--
-- 15 folding
--
-- foldenable	unset to display all folds open
-- 	(local to window)
--  	set fen	nofen
-- foldlevel	folds with a level higher than this number will be closed
-- 	(local to window)
--  	set fdl=99
-- foldlevelstart	value for 'foldlevel' when starting to edit a file
--  	set fdls=-1
-- foldcolumn	width of the column used to indicate folds
-- 	(local to window)
--  	set fdc=0
-- foldtext	expression used to display the text of a closed fold
-- 	(local to window)
--  	set fdt=
-- foldclose	set to "all" to close a fold when the cursor leaves it
--  	set fcl=
-- foldopen	specifies for which commands a fold will be opened
--  	set fdo=block,hor,mark,percent,quickfix,search,tag,undo
-- foldminlines	minimum number of screen lines for a fold to be closed
-- 	(local to window)
--  	set fml=1
-- foldmethod	folding type: "manual", "indent", "expr", "marker",
-- 	"syntax" or "diff"
-- 	(local to window)
--  	set fdm=expr
-- foldexpr	expression used when 'foldmethod' is "expr"
-- 	(local to window)
--  	set fde=v:lua.vim.lsp.foldexpr()
-- foldignore	used to ignore lines when 'foldmethod' is "indent"
-- 	(local to window)
--  	set fdi=#
-- foldmarker	markers used when 'foldmethod' is "marker"
-- 	(local to window)
--  	set fmr={{{,}}}
-- foldnestmax	maximum fold depth for when 'foldmethod' is "indent" or "syntax"
-- 	(local to window)
--  	set fdn=20
--
-- 16 diff mode
--
-- diff	use diff mode for the current window
-- 	(local to window)
--  	set nodiff	diff
-- diffopt	options for using diff mode
--  	set dip=internal,filler,closeoff,indent-heuristic,inline:char,linematch:40
-- diffexpr	expression used to obtain a diff file
--  	set dex=
-- diffanchors	list of addresses for anchoring a diff
-- 	(global or local to buffer)
--  	set dia=
-- patchexpr	expression used to patch a file
--  	set pex=
--
-- 17 mapping
--
-- maxmapdepth	maximum depth of mapping
--  	set mmd=1000
-- timeout	allow timing out halfway into a mapping
--  	set to	noto
-- ttimeout	allow timing out halfway into a key code
--  	set ttimeout	nottimeout
-- timeoutlen	time in msec for 'timeout'
--  	set tm=300
-- ttimeoutlen	time in msec for 'ttimeout'
--  	set ttm=50
--
-- 18 reading and writing files
--
-- modeline	enable using settings from modelines when reading a file
-- 	(local to buffer)
--  	set ml	noml
-- modelineexpr	allow setting expression options from a modeline
--  	set nomle	mle
-- modelines	number of lines to check for modelines
--  	set mls=5
-- binary	binary file editing
-- 	(local to buffer)
--  	set nobin	bin
-- endofline	last line in the file has an end-of-line
-- 	(local to buffer)
--  	set eol	noeol
-- endoffile	last line in the file followed by CTRL-Z
-- 	(local to buffer)
--  	set noeof	eof
-- fixendofline	fixes missing end-of-line at end of text file
-- 	(local to buffer)
--  	set fixeol	nofixeol
-- bomb	prepend a Byte Order Mark to the file
-- 	(local to buffer)
--  	set nobomb	bomb
-- fileformat	end-of-line format: "dos", "unix" or "mac"
-- 	(local to buffer)
--  	set ff=unix
-- fileformats	list of file formats to look for when editing a file
--  	set ffs=unix,dos
-- write	writing files is allowed
--  	set write	nowrite
-- writebackup	write a backup file before overwriting a file
--  	set nowb	wb
-- backup	keep a backup after overwriting a file
--  	set nobk	bk
-- backupskip	patterns that specify for which files a backup is not made
--  	set bsk=/private/tmp/*,/var/folders/3g/dm7sf7nn0c5105x56dr89bkh0000gn/T/*
-- backupcopy	whether to make the backup as a copy or rename the existing file
-- 	(global or local to buffer)
--  	set bkc=auto
-- backupdir	list of directories to put backup files in
--  	set bdir=.,/Users/boran/.local/state/nvim/backup//
-- backupext	file name extension for the backup file
--  	set bex=~
-- autowrite	automatically write a file when leaving a modified buffer
--  	set aw	noaw
-- autowriteall	as 'autowrite', but works with more commands
--  	set noawa	awa
-- writeany	always write without asking for confirmation
--  	set nowa	wa
-- autoread	automatically read a file when it was modified outside of Vim
-- 	(global or local to buffer)
--  	set ar	noar
-- patchmode	keep oldest version of a file; specifies file name extension
--  	set pm=
-- fsync	forcibly sync the file to disk after writing it
-- 	(global or local to buffer)
--  	set fs	nofs
--
-- 19 the swap file
--
-- directory	list of directories for the swap file
--  	set dir=/Users/boran/.local/state/nvim/swap//
-- swapfile	use a swap file for this buffer
-- 	(local to buffer)
--  	set noswf	swf
-- updatecount	number of characters typed to cause a swap file update
--  	set uc=200
-- updatetime	time in msec after which the swap file will be updated
--  	set ut=300
--
-- 20 command line editing
--
-- history	how many command lines are remembered
--  	set hi=10000
-- wildchar	key that triggers command-line expansion
--  	set wc=9
-- wildcharm	like 'wildchar' but can also be used in a mapping
--  	set wcm=0
-- wildmode	specifies how command line completion works
--  	set wim=longest:full,full
-- wildoptions	empty or "tagfile" to list file name of matching tags
--  	set wop=pum,tagfile
-- suffixes	list of file name extensions that have a lower priority
--  	set su=.bak,~,.o,.h,.info,.swp,.obj
-- suffixesadd	list of file name extensions added when searching for a file
-- 	(local to buffer)
--  	set sua=.lua
-- wildignore	list of patterns to ignore files for file name completion
--  	set wig=
-- fileignorecase	ignore case when using file names
--  	set fic	nofic
-- wildignorecase	ignore case when completing file names
--  	set nowic	wic
-- wildmenu	command-line completion shows a list of matches
--  	set wmnu	nowmnu
-- cedit	key used to open the command-line window
--  	set cedit=
-- cmdwinheight	height of the command-line window
--  	set cwh=7
--
-- 21 executing external commands
--
-- shell	name of the shell program used for external commands
--  	set sh=/opt/homebrew/bin/fish
-- shellquote	character(s) to enclose a shell command in
--  	set shq=
-- shellxquote	like 'shellquote' but include the redirection
--  	set sxq=
-- shellxescape	characters to escape when 'shellxquote' is (
--  	set sxe=
-- shellcmdflag	argument for 'shell' to execute a command
--  	set shcf=-c
-- shellredir	used to redirect command output to a file
--  	set srr=>%s 2>&1
-- shelltemp	use a temp file for shell commands instead of using a pipe
--  	set nostmp	stmp
-- equalprg	program used for "=" command
-- 	(global or local to buffer)
--  	set ep=
-- formatprg	program used to format lines with "gq" command
-- 	(global or local to buffer)
--  	set fp=
-- keywordprg	program used for the "K" command
-- 	(global or local to buffer)
--  	set kp=:Man
-- warn	warn when using a shell command and a buffer has changes
--  	set warn	nowarn
--
-- 22 running make and jumping to errors (quickfix)
--
-- errorfile	name of the file that contains error messages
--  	set ef=errors.err
-- errorformat	list of formats for error messages
-- 	(global or local to buffer)
--  	set efm=%*[^"]"%f"%*\D%l: %m,"%f"%*\D%l: %m,%-Gg%\?make[%*\d]: *** [%f:%l:%m,%-Gg%\?make: *** [%f:%l:%m,%-G%f:%l: (Each undeclared identifier is reported only once,%-G%f:%l: for each function it appears in.),%-GIn file included from %f:%l:%c:,%-GIn file included from %f:%l:%c\,,%-GIn file included from %f:%l:%c,%-GIn file included from %f:%l,%-G%*[ ]from %f:%l:%c,%-G%*[ ]from %f:%l:,%-G%*[ ]from %f:%l\,,%-G%*[ ]from %f:%l,%f:%l:%c:%m,%f(%l):%m,%f:%l:%m,"%f"\, line %l%*\D%c%*[^ ] %m,%D%*\a[%*\d]: Entering directory %*[`']%f',%X%*\a[%*\d]: Leaving directory %*[`']%f',%D%*\a: Entering directory %*[`']%f',%X%*\a: Leaving directory %*[`']%f',%DMaking %*\a in %f,%f|%l| %m
-- makeprg	program used for the ":make" command
-- 	(global or local to buffer)
--  	set mp=make
-- shellpipe	string used to put the output of ":make" in the error file
--  	set sp=2>&1| tee
-- makeef	name of the errorfile for the 'makeprg' command
--  	set mef=
-- grepprg	program used for the ":grep" command
-- 	(global or local to buffer)
--  	set gp=rg --vimgrep
-- grepformat	list of formats for output of 'grepprg'
-- 	(global or local to buffer)
--  	set gfm=%f:%l:%c:%m
-- makeencoding	encoding of the ":make" and ":grep" output
-- 	(global or local to buffer)
--  	set menc=
-- quickfixtextfunc
-- 	function to display text in the quickfix window
--  	set qftf=
--
-- 23 system specific
--
-- shellslash	use forward slashes in file names; for Unix-like shells
--  	set ssl	nossl
-- completeslash	specifies slash/backslash used for completion
-- 	(local to buffer)
--  	set csl=
--
-- 24 language specific
--
-- isfname	specifies the characters in a file name
--  	set isf=@,48-57,/,.,-,_,+,,,#,$,%,~,=
-- isident	specifies the characters in an identifier
--  	set isi=@,48-57,_,192-255
-- iskeyword	specifies the characters in a keyword
-- 	(local to buffer)
--  	set isk=@,48-57,_,192-255
-- isprint	specifies printable characters
--  	set isp=@,161-255
-- quoteescape	specifies escape characters in a string
-- 	(local to buffer)
--  	set qe=\
-- rightleft	display the buffer right-to-left
-- 	(local to window)
--  	set norl	rl
-- rightleftcmd	when to edit the command-line right-to-left
-- 	(local to window)
--  	set rlc=search
-- revins	insert characters backwards
--  	set nori	ri
-- allowrevins	allow CTRL-_ in Insert and Command-line mode to toggle 'revins'
--  	set noari	ari
-- arabic	prepare for editing Arabic text
-- 	(local to window)
--  	set noarab	arab
-- arabicshape	perform shaping of Arabic characters
--  	set arshape	noarshape
-- termbidi	terminal will perform bidi handling
--  	set notbidi	tbidi
-- keymap	name of a keyboard mapping
-- 	(local to buffer)
--  	set kmp=
-- langmap	list of characters that are translated in Normal mode
--  	set lmap=
-- langremap	apply 'langmap' to mapped characters
--  	set nolrm	lrm
-- iminsert	in Insert mode: 1: use :lmap; 2: use IM; 0: neither
-- 	(local to buffer)
--  	set imi=0
-- imsearch	entering a search pattern: 1: use :lmap; 2: use IM; 0: neither
-- 	(local to buffer)
--  	set ims=-1
--
-- 25 multi-byte characters
--
-- fileencoding	character encoding for the current file
-- 	(local to buffer)
--  	set fenc=utf-8
-- fileencodings	automatically detected character encodings
--  	set fencs=ucs-bom,utf-8,default,latin1
-- charconvert	expression used for character encoding conversion
--  	set ccv=
-- delcombine	delete combining (composing) characters on their own
--  	set nodeco	deco
-- ambiwidth	width of ambiguous width characters
--  	set ambw=single
-- emoji	emoji characters are full width
--  	set emo	noemo
--
-- 26 various
--
-- virtualedit	when to use virtual editing: "block", "insert", "all"
-- 	and/or "onemore"
-- 	(global or local to window)
--  	set ve=all
-- eventignore	list of autocommand events which are to be ignored
--  	set ei=
-- eventignorewin	list of autocommand events which are to be ignored in a window
-- 	(local to window)
--  	set eiw=
-- loadplugins	load plugin scripts when starting up
--  	set nolpl	lpl
-- exrc	enable reading .vimrc/.exrc/.gvimrc in the current directory
--  	set noex	ex
-- maxfuncdepth	maximum depth of function calls
--  	set mfd=100
-- sessionoptions	list of words that specifies what to put in a session file
--  	set ssop=buffers,curdir,tabpages,winsize,help,globals,skiprtp,folds
-- viewoptions	list of words that specifies what to save for :mkview
--  	set vop=folds,cursor,curdir
-- viewdir	directory where to store files with :mkview
--  	set vdir=/Users/boran/.local/state/nvim/view//
-- shada	list that specifies what to write in the ShaDa file
--  	set sd=!,'100,<50,s10,h,r/tmp/,r/private/
-- shadafile	overrides the filename used for shada
--  	set sdf=
-- bufhidden	what happens with a buffer when it's no longer in a window
-- 	(local to buffer)
--  	set bh=
-- buftype	empty, "nofile", "nowrite", "quickfix", etc.: type of buffer
-- 	(local to buffer)
--  	set bt=
-- buflisted	whether the buffer shows up in the buffer list
-- 	(local to buffer)
--  	set bl	nobl
-- debug	set to "msg" to see all error messages
--  	set debug=
-- signcolumn	whether to show the signcolumn
-- 	(local to window)
--  	set scl=number
-- pyxversion	whether to use Python 2 or 3
--  	set pyx=3
-- inccommand	live preview of substitution
--  	set icm=nosplit
-- busy	buffer is busy
-- 	(local to buffer)
--  	set busy=0
-- termpastefilter
-- 	characters removed when pasting into terminal window
--  	set tpf=BS,HT,ESC,DEL
-- scrollback	number of lines kept beyond the visible screen in terminal buffer
-- 	(local to buffer)
--  	set scbk=-1
-- ttyfast	assume terminal responds quickly, enabling more features
--  	set tf	notf
