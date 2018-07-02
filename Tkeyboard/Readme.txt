TKeyboard VCL Component
=======================

Version: 1.0.0


DESCRIPTION

TKeyboard displays a virtual on-screen keyboard and can completely replace 
a hardware keyboard. TKeyboard is especially usefull when writing Touch-Screen
applications. TKeyboard is fully object oriented. Every key property can be accessed
and modified at runtime, including disabling or hiding the key.

Keyboard Modes
To fully substitute a hardware keyboard, TKeyboard supports three keyboard
modes: Normal, Shift and AltGR. In Normal mode Tkeyboard displays the
standard key captions of a normal keyboard. In shift mode TKeyboard displays
the shift key captions (i.e. $ instead of 4). In AtlGR mode TKeyboard displays
the key captions for keys with an AltGr caption defined. AltGr is a special key
on some european keyboards to generate special symols such as the german umlauts.
Pressing a key in a certain mode will also generate the corresponding key value
for that mode. The key caption and the key value for each mode can be set
using the layout editor.
Shift, Caps lock and AltGr: The behavior of the shift key is slightly different than on
real keyboards. Pressing shift once puts TKeyboard in shift mode and TKeyboard
displays the second function of the keys. After pressing one key TKeyboard falls
back in normal mode. The same is true for the AltGr key. Caps Lock works just the
same way it does on a real keyboard.

Layout Editor
TKeyboard comes with a build in, design time, wysiwyg layout editor. Using the
layout editor every type of keyboard (standard, with/without function keys,
numeric, etc.) can be defined by starting out with a blank canvas. New keys can 
be added to the layout and positioned on the canvas using the mouse. 
Each key can be customized using the following properties:

- Key Type: Normal, Shift, Caps Lock, Escape, Alt Gr, Enter, Tabulator, Backspace,
            Insert, Delete, Home, End, Page Up, Page Down, Left, Right, Up, Down, Function
- Key Width
- Key Height
- Key Captions for Normal, Shift and Alt Gr modes
- Key Values for Normal, Shift and Alt Gr modes
- Key Font: all font properties can be set

Layouts can by saved and loaded to and from layout files using the layout editor or
at runtime using special functions. TKeyboard comes with
predefined layout files for standard US and german keyboards as
well as layouts for numeric and calculator keyboards.
		
TKeyboard properties
The CharCase property controls the case of the returned characters. On ecUpper
all characters are converted to uppercase, on ecLower all characters are
converted to lowercase.
Through the LinkedControl property TKeyboard can be linked to any descendant of
TWinControl. By linking a control to TKeyboard, key codes for keys pressed on
TKeyboard, navigation keys included, are passed on to the linked control which
will react accordingly. This is especially usefull for TEdit, TMemo, TRichtText
and TComboBox (and the corresponding ...DB.. controls).

PORTABILITY
TKeyboard has been tested on Delphi 5 and 6. TKeyboard has not been tested on
any version of CBuilder and/or Kylix.


INSTALLATION
Before installing this version of TKeyboard make sure you deinstall any previous version
of TKeyboard and remove any keyboard.pas or keyboard.dcu file from your library path.

Delphi 7
Copy KeyboardDN7.bpl and KeyboardRT7.bpl to your Delphi7/bin directory or to some
directory on your path. Copy keyboard.dcu, KeyboardDN7.dcp and KeyboardRT7.dcp to
Delphi7/lib directory or to some directory in your library path.

Delphi 6
Copy KeyboardDN6.bpl and KeyboardRT6.bpl to your Delphi6/bin directory or to some
directory on your path. Copy keyboard.dcu, KeyboardDN6.dcp and KeyboardRT6.dcp to
Delphi6/lib directory or to some directory in your library path.

Delphi 5
Copy KeyboardDN5.bpl and KeyboardRT5.bpl to your Delphi5/bin directory or to some
directory on your path. Copy keyboard.dcu, KeyboardDN5.dcp and KeyboardRT5.dcp to
Delphi5/lib directory or to some directory in your library path.


TRIAL VERSION
The trial version of TKeyboard is fully functional in design time as well as in
runtime mode. A nag screen will be displayed in runtime mode if the Delphi IDE
is not running.

LICENSE & REGISTRATION
TKeyboard is distributed as Shareware. To register TKeyboard please visit
http://www.sarix.biz/tkeyboard. Registered users get the full version of TKeyboard
(including full source code) and an unlimited deployment license. TKeyboard licensing
is per seat, meaning a license is required for every developer using TKeyboard. Volume 
discounts and site licenses are available. 
TKeyboard design time packages as well as source and object code in any form
(including but not limited to .PAS, .DCU, .OBJ), in whole or in part, modified or
unmodified, may not be redistributed for profit or as part of another commercial
or shareware software package without express written permission from me.
TKeyboard may not be used to develop a commercially competitive product.

DEPLOYMENT
Registered users are allowed to deploy the following files as part of their software.
KeyboardRT7.bpl
KeyboardRT6.bpl
KeyboardRT5.bpl
No other files may be deployed.


LIABILITY
ALL WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO THIS SOFTWARE,
INCLUDING WITHOUT LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND IN NO EVENT SHALL AUTHORS BE
LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, TORT (INCLUDING NEGLIGENCE) OR
STRICT LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

HISTORY
Version 1.0.0: First release

CONTACT:
For support, bug reports or suggestions please write to

	Martin Geier
	martin.geier@sarix.biz
	http://www.sarix.biz/tkeyboard


Copyright: Martin Geier, 2002

