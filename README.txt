                      MEDIA COLLECTION CATALOGUE
                      --------------------------

1. Introduction

The main purpose of this package is to keep and manage simple media
collection catalogue providing easy data access and control means from
within Emacs. Initially it was designed for keeping CD/DVD collection,
but nothing prevents it from using with another content, such as paper
books or something else. Nevertheless, there is an extra advantage
especially for CD and DVD, since these media types can be
automatically identified and recognized being put into the drive.


2. Installation

There is no need to read this section if you've already installed
Catalogue as a Debian package and you may safely skip to the next
one. But if you prefer to do it manually, so the necessary steps are
pretty simple. Place all this stuff as is wherever you wish and put
into your ~/.emacs the following string:

(load "/somewhere/catalogue/lisp/catalogue")

Of course, the word "somewhere" must be replaced by a real path to
the package. In addition you may byte-compile all files in the lisp
subdirectory if you like.


3. Usage

Press "C-c v" to enter catalogue data display buffer. If catalogue is
not empty the first index card will be presented, otherwise you will
be prompted to add new items. In this buffer you can browse and search
catalogue, add new items and edit existing ones. Each item is
identified by it's name, category and unit number in set. For
instance, if you have some music album consisting from two audio CDs,
each disk can (and should) be registered separately: both with quite
the same name and category, but different unit number.

All items in catalogue are sorted by these fields in alphabetical
order for convenient navigation.

The data display buffer has it's own keymap where special key strokes
are defined and the most of available functions are accessible via
Catalogue submenu in the main menu.

To kill this buffer and to quit catalogue press "q".

The most other useful functions are described below. And, of course,
you can always press "C-h m" or "F1 m" to get exhaustive online help
on all available key strokes.


3.1. Browsing

In the catalogue data display buffer you can browse index cards one by
one pressing PGDN and PGUP navigation keys or by item sets using
these keys with prefix argument. Also you can jump straight to the
next or previous category by pressing CTRL-PGDN and CTRL-PGUP
respectively. and, in addition, CTRL-HOME and CTRL-END bring you to
the first and last item in the catalogue.

Alternatively you can use "n" and "p" to go to the next or previous
item and "C-n" and "C-p" to jump to the next or previous category.


3.2. Searching

To search an item by field content press "s" in the data display
buffer or choose this function from the Catalogue menu. You will be
prompted for field that can be chosen from completions and search
pattern. In the simplest case the search pattern is treated as a
substring that may optionally be preceded by a comparison operator
"<", ">" or "=" or as a regular expression if it is preceded by
"/". These elementary patterns may be combined by logical operators
"AND", "OR", and "NOT". Searching process starts from the item next to
the current one and goes forward wrapping around automatically. If it
succeeds the first found item becomes the current one.


3.3. Editing

To enter edit mode press "e" or ENTER. You will be presented by the
list of all editable fields with their values for current item. You
may edit these values in the usual way using arrow keys to move across
the fields. While editing a field represented by single text string
you may press ENTER and make use of alternative input method providing
per field history and for some fields completions as well. Pressing
ENTER within the multiline description field inserts a new line as
usual. For numeric fields, such as set and unit, this key stroke is
meaningless.

While editing the program automatically takes care of some constraints
applied to prevent you from occasional data integrity corruption. For
instance, name and category must not be empty and unit number must not
exceed total number of units in set. So, this latter value is
automatically increased if necessary and any it's change always
spreads over the full item set.

Although navigation functions described in 3.1 are generally available
in this mode, so you can edit several items at a time, the program
will prevent you from leaving an index card in incorrect state
blocking these capabilities in such cases.

When you are done with editing press "C-c C-c" to commit changes. Or
you can discard all changes and return to the catalogue view mode by
pressing "C-c q".


3.4. Adding new items

To add a new item press "a" in the catalogue view mode and then fill
the blank index card editing it as it was described above.


3.5. Registering and identifying disks

This function is bound to the global key stroke "C-c d", Thus, it is
available from anywhere, but in the data display buffer you can use
"/" or "I" as well.

To identify a disk put it into the drive and press "C-c d" or "/" if
you are in the data display buffer. If this disk is already registered
in the catalogue it's index card will be found and shown in the data
display buffer. Otherwise you will be prompted to register it filling
out the index card. Since some information about the disk will be
already gathered up to this moment, more or less reasonable values for
some fields will be suggested. For instance, media type is always
detected automatically. For audio disks additional information is
retreived from the cdtool local database via cdir utility, so it seems
to be wise to add an audio disk to this database using cdown and cdadd
utilities. For data disk some information might be extracted from the
disk label and directory tree, and so on.

Furthermore, you may reassign currently displayed index card to any
other disk by inserting it into the drive and pressing "C-c r". If
this disk appears to be already registered in the catalogue you will
be asked for additional confirmation.


3.6. Deleting items

To delete current item press DELETE or "C-d" in the data display
buffer. If prefix argument is in effect then full item set will be
deleted.


3.7. Changing item status

Each item in the catalogue is characterized by it's status that may be
changed. Some additional functions especially devoted to this purpose
will be described below. All these functions operate on the current
item in the data display buffer or on the full item set being invoked
with prefix argument.

At first, any item can be either native or alien that is owned by
whomever else. This status depends on the value of field owner. If it
is empty the item is respected as native. A native item becomes alien
when you give it up by pressing "G" and entering new owner. An alien
item in turn becomes native when you acquire it by pressing "A". Of
course, these transitions can be accomplished by direct editing as
well.

Besides that, any item may be lended to someone, to give it this
status press "L" and enter borrower name, and an alien item may be
marked as borrowed by pressing "B".

At last, any borrowed or lended item may be released by pressing "R".

All these functions are also accessible via Catalogue submenu in the
main menu.


3.8. Summary view

Pressing "S" in the data display buffer you can switch to the summary
view that is popped up in a separate window. Here each item is
represented briefly by one line, so you can see a bunch of items at
once and move from one item to another by vertical arrows. The PGUP
and PGDN keys are used here for scrolling over the full window. Both
buffers work in conjunction and are always kept synchronized. All
functions that were described above are available from the summary
view as well and are mostly bound to the same key strokes, but there
are some differences and advanced features that will be discussed
further. To close summary buffer press "q" in it.

Of course, the most of functions available in the summary buffer are
accessible via Catalogue submenu in the main menu as well.


3.8.1. Marking

Any item in the summary buffer may be marked that is reflected by the
"+" sign placed at the beginning of the line. This state is volatile
in the sense that it is actual only for session and is not stored in
the database.

You can mark an item in the summary buffer by pressing "m" and unmark
it by pressing "u". With prefix argument the full item set will be
affected. To mark and unmark all items in the category use "C-c m" and
"C-c u" respectively. All marked items can be unmarked by pressing
"U".

In the summary buffer you can navigate through marked items forth and
back by CTRL-DOWN and CTRL-UP or "C-n" and "C-p".

All functions described in 3.6 and 3.7 being invoked in the summary
buffer do not respect prefix argument, but operate on the marked items
if any or on the current one if no marks are in effect.


3.8.2. Filtering

There are four filter functions available in the summary buffer that
allow to select items by their status:

"F n" -- native;
"F a" -- alien;
"F b" -- borrowed;
"F l" -- lended.

These filters are applied to the marked items if any or to all ones if
no marks are in effect. After applying a filter, only those items that
satisfy filter condition remain marked.


3.8.3. Searching

The searching mechanism described in 3.2 is available in the summary
buffer as well, but at the contrary to the data display buffer, here
all found items become marked and before starting search all existing
marks are cleared.

In addition, forward and backward incremental search functions are
also available here. As usual, these functions are bound to "C-s" and
"C-r" respectively. Incremental search does not affect marks.


3.8.4. Reports

From the summary buffer you can make a brief report of the marked
items if any or of the full catalogue content if no marks are in
effect. This report is created in it's own buffer and shown in a
separate window. Each item set is represented in the report by one
entry. To make a report press "r" in the summary buffer.


4. Customizing

Some aspects of the program behaviour can be customized in the group
Catalogue that becomes available right after the package installation
and restarting Emacs.
