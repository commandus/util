unit
  utilVideo;
(*##*)
(*******************************************************************************
*                                                                             *
*   U  T  I  L  V  I  D  E  O                                                  *
*                                                                             *
*   Copyright © 2006- 2006, Andrei Ivanov. All rights reserved.                *
*                                                                             *
*                                                                              *
*   Video for Windows helper functions                                        *
*   Conditional defines:                                                       *
*                                                                             *
*   Last Revision: Dec 01 2006                                                 *
*   Last fixes   : Oct 01 2007                                                *
*                                                                              *
*                                                                             *
*                                                                              *
*                                                                             *
*   Lines        : 62                                                          *
*   History      : see CHANGES.TXT file                                       *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface
uses
  Classes, VFW;

/// return count of installed video devices. 0 if none
/// AStrings returns names of video devices
function GetVideoDeviceList(AStrings: TStrings): Integer;

implementation

/// return count of installed video devices. 0 if none
/// AStrings returns names of video devices. Can be Nil
function GetVideoDeviceList(AStrings: TStrings): Integer;
var
  DeviceName,
  DeviceVersion: String;
begin
  if Assigned(AStrings)
  then AStrings.Clear;
  SetLength(DeviceName, 1024);
  SetLength(DeviceVersion, 1024);
  Result:= 0;
  repeat
    if vfw.capGetDriverDescription(Result, PChar(DeviceName),
      Length(DeviceName), PChar(DeviceVersion), Length(DeviceVersion)) then begin
      // Append name to list of installed capture drivers and then let the user select a driver to use.
      SetLength(DeviceName, Length(PChar(DeviceName)));
      SetLength(DeviceVersion, Length(PChar(DeviceVersion)));
      if Assigned(AStrings)
      then AStrings.Add(DeviceName); //  + ' ' + DeviceVersion
      Inc(Result);
    end else Break;
  until False;
end;

end.
