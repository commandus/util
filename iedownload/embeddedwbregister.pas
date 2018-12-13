unit
  embeddedwbregister;

interface

uses
  Classes, IEConst, EmbeddedWB;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TEmbeddedWB]);
end;

end.
