{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit general;

{$warn 5023 off : no warning about unused units}
interface

uses
  ubenchmark, ucomport, uError, uExtControls, uInfo, ProcessUtils, 
  uLanguageUtils, PoTranslator, UtilsVis, uColors, uImaging, uwait, uLogWait, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uExtControls', @uExtControls.Register);
end;

initialization
  RegisterPackage('general', @Register);
end.
