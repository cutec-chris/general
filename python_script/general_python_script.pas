{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit general_python_script;

{$warn 5023 off : no warning about unused units}
interface

uses
  genpythonscript, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('general_python_script', @Register);
end.
