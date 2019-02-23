unit UtilityUnit;

{ GDAnnotate - utility functions                                                }
{ CopyLeft Geriadiode, 12th Feb 2019.                                           }
{ Licence: Open Source LGPL/GPL                                                 }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Math, Types;

type
  TTextGraphicShape = (shNone, shRectangle, shRoundRect, shCircle, shEllipse, shCloud, shScroll);

procedure MakeShape(var Bitmap: TBGRABitmap; Shape: TTextGraphicShape; Text: string;
  BackGroundColor: TBGRAPixel);

implementation

procedure MakeShape(var Bitmap: TBGRABitmap; Shape: TTextGraphicShape; Text: string;
  BackGroundColor: TBGRAPixel);
var
  TmpBitmap: TBGRABitmap;
  TextSize, ExpTextSize: TSize;
  mx: Integer;
begin
  if (Text <> '') and Assigned(Bitmap) then begin
    TextSize := Bitmap.TextSize(Text);
    ExpTextSize.cx := Round(TextSize.cx + (TextSize.cx/10));
    Bitmap.SetSize(ExpTextSize.cx, TextSize.cy);
    case Shape of
      shRectangle: Bitmap.Rectangle(0, 0, Bitmap.Width, Bitmap.Height, BackgroundColor,
        BackgroundColor, dmDrawWithTransparency);
      shRoundRect: Bitmap.RoundRect(0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Height div 2,
        Bitmap.Height div 2, BackgroundColor, BackgroundColor, dmDrawWithTransparency);
      shCircle: begin
        mx := Max(Bitmap.Width, Bitmap.Height);
        Bitmap.SetSize(mx, mx);
        Bitmap.FillEllipseAntiAlias(mx/2, mx/2, mx/2, mx/2, BackgroundColor);
      end;
      shEllipse: Bitmap.FillEllipseAntiAlias(Bitmap.Width div 2, Bitmap.Height div 2,
        Bitmap.Width div 2, Bitmap.Height div 2, BackgroundColor);
      shCloud: begin
        TmpBitmap := TBGRABitmap.Create;
        TmpBitmap.LoadFromResource('CLOUD');
        TmpBitmap.FloodFill(TmpBitmap.Width div 2, TmpBitmap.Height div 2, BackgroundColor, fmSet, 200);
        Bitmap.StretchPutImage(Bitmap.ClipRect, TmpBitmap, dmDrawWithTransparency, 255);
        TmpBitmap.Free;
      end;
      shScroll: begin
        TmpBitmap := TBGRABitmap.Create;
        TmpBitmap.LoadFromResource('SCROLL');
        TmpBitmap.FloodFill(TmpBitmap.Width div 2, TmpBitmap.Height div 2, BackgroundColor, fmSet, 200);
        Bitmap.StretchPutImage(Bitmap.ClipRect, TmpBitmap, dmDrawWithTransparency, 255);
        TmpBitmap.Free;
      end;
    end;
  end;
end;

end.

