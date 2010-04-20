;;;; Caps Lock to Control
CapsLock::Ctrl
RCtrl::AppsKey

;;;; Shift + Scroll as Horizontal Scroll in Firefox
~LShift & WheelUp:: ; Scroll left.
if WinActive("ahk_class MozillaUIWindowClass")
{
   Loop 5  ; Scroll Speed
   send,{left}
}
else
{
   ControlGetFocus, control, A
   Loop 10  ; Scroll Speed
   SendMessage, 0x114, 0, 0, %control%, A ; 0x114 is WM_HSCROLL
}
return

~LShift & WheelDown:: ; Scroll right.
if WinActive("ahk_class MozillaUIWindowClass")
{
   Loop 5  ; Scroll Speed
   send,{right}
}
else
{
   ControlGetFocus, control, A
   Loop 10  ; Scroll Speed
   SendMessage, 0x114, 1, 0, %control%, A ; 0x114 is WM_HSCROLL
}
return
