unit ModelUnit;

{ This Model contains the classes that make up the application's business logic.}
{ In this instance we define a text-based TAnnotation class which has:          }
{ * A position reference that determine where it will appear in the GUI window. }
{ * A choice of any installed font in any available size and color.  The font   }
{   color can be set to any opacity between opaque and transparent.             }
{ * An optional background shape (Rectangle, Rounded Rectangle, Ellipse,Cloud,  }
{   etc.  This shape's fill color can be set to any opacity between opaque and  }
{   transparent.                                                                }

{ CopyLeft Geriadiode, 12th Feb 2019.                                           }
{ Licence: Open Source LGPL/GPL                                                 }

{$mode delphi}

interface

uses
  {$I Synopse.inc}
  Classes, SysUtils, Graphics, Types, BGRABitmap, BGRABitmapTypes, SynCommons,
  mORMot, dialogs, lclproc, UtilityUnit;

type

  TAnnotation = class(TSQLRecord)
  private
    FX: Integer;
    FY: Integer;
    FLeft: Integer;
    FTop: Integer;
    FBitmap: TBGRABitmap;
    FAnnotationType: string;
    FWidth: Integer;
    FHeight: Integer;
    FText: RawUTF8;
    FFontName: string;
    FFontHeight: Integer;
    FFontFullHeight: Integer;
    FFontQuality: TBGRAFontQuality;
    FFontAntiAlias: Boolean;
    FFontStyle: TFontStyles;
    FFontPitch: TFontPitch;
    FFontOrientation: Integer;
    FFontColorString: string;
    FBackgroundColor: TColor;
    FBackgroundColorString: string;
    FOwnerFilename: string;
    FShape: TTextGraphicShape;
    procedure PaintBackground(Shape: TTextGraphicShape);
    procedure SetX(Value: Integer);
    procedure SetY(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetText(Value: RawUTF8);
    procedure SetFontName(Value: string);
    procedure SetFontStyle(Value: TFontStyles);
    procedure SetFontOrientation(Value: Integer);
    procedure SetFontHeight(Value: Integer);
    procedure SetFontFullHeight(Value: Integer);
    procedure SetFontQuality(Value: TBGRAFontQuality);
    procedure SetFontAntiAlias(Value: Boolean);
    procedure SetFontColorString(Value: string);
    procedure SetBackgroundColorString(Value: string);
    procedure SetOwnerFilename(Value: string);
    procedure SetShape(Value: TTextGraphicShape);
    procedure SetAnnotationType(Value: string);
    procedure SetBitmap(Value: TBGRABitmap);
    procedure ResizeBitmap;
  public
    // Private and Public properties are ignored by the persistence mechanism.
    // We don't need to persist the object's bitmap - it is processed locally.
    property Bitmap: TBGRABitmap read FBitmap write SetBitmap;
    // Invalidate procedure repaints the BGRABitmap on which the text is rendered.
    procedure Invalidate;
    constructor Create; override;
    destructor Destroy; override;
  published
    // Only properties with 'published' visibility get persisted by mORMot.
    // X and Y define the centre of the object
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
    // The top/left corner needs its own variables
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property AnnotationType: string read FAnnotationType write SetAnnotationType;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Text: RawUTF8 read FText write SetText;
    //BGRA font parameters
    property FontName: string read FFontName write SetFontName;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property FontOrientation: Integer read FFontOrientation write SetFontOrientation;
    property FontHeight: Integer read FFontHeight write SetFontHeight;
    property FontFullHeight: Integer read FFontFullHeight write SetFontFullHeight;
    property FontQuality: TBGRAFontQuality read FFontQuality write SetFontQuality;
    property FontAntiAlias: Boolean read FFontAntiAlias write SetFontAntiAlias;
    // BGRAPixel colors are handled as strings because FPC cannot (yet) generate RTTI on records.
    property FontColorString: string read FFontColorString write SetFontColorString;
    property BackgroundColorString: string read FBackgroundColorString write SetBackgroundColorString;
    // Associate the annottion with document that will display it
    property OwnerFilename: string read FOwnerFilename write SetOwnerFilename;
    property Shape: TTextGraphicShape read FShape write SetShape;
  end;

implementation

{ TAnnotation }

procedure TAnnotation.SetX(Value: Integer);
begin
  if FX <> Value then begin
    FX := Value;
    Invalidate;
  end;
end;

procedure TAnnotation.SetY(Value: Integer);
begin
  if FY <> Value then begin
    FY := Value;
    Invalidate;
  end;
end;

procedure TAnnotation.SetLeft(Value: Integer);
begin
  if FLeft <> Value then
    FLeft := Value;
end;

procedure TAnnotation.SetTop(Value: Integer);
begin
  if FTop <> Value then
    FTop := Value;
end;

procedure TAnnotation.SetText(Value: RawUTF8);
begin
  if FText <> Value then begin
    FText := Value;
    if Assigned(FBitmap) then begin
      ResizeBitmap;
    end;
  end;
end;

// We don't attempt to persist an entire TBGRAFont in one go.  We just need to persist its
// properties (which will end up in individual database table fields) ...
procedure TAnnotation.SetFontName(Value: string);
begin
  if FFontName <> Value then begin
    FFontName := Value;
    FBitmap.FontName := FFontName;
    ResizeBitmap;
  end;
end;

procedure TAnnotation.SetFontHeight(Value: Integer);
begin
  if FFontHeight <> Value then begin
    FFontHeight := Value;
    FBitmap.FontHeight := FFontHeight;
    ResizeBitmap;
  end;
end;

procedure TAnnotation.SetFontFullHeight(Value: Integer);
begin
  if FFontFullHeight <> Value then begin
    FFontFullHeight := Value;
    FBitmap.FontFullHeight := FFontFullHeight;
    ResizeBitmap;
  end;
end;

procedure TAnnotation.SetFontQuality(Value: TBGRAFontQuality);
begin
  if FFontQuality <> Value then begin
    FFontQuality := Value;
    FBitmap.FontQuality := FFontQuality;
    ResizeBitmap;
  end;
end;

procedure TAnnotation.SetFontAntiAlias(Value: Boolean);
begin
  if FFontAntiAlias <> Value then begin
    FFontAntiAlias := Value;
    FBitmap.FontAntiAlias := FontAntiAlias;
    ResizeBitmap;
  end;
end;

procedure TAnnotation.SetFontStyle(Value: TFontStyles);
begin
  if FFontStyle <> Value then begin
    FFontStyle := Value;
    FBitmap.FontStyle := FFontStyle;
    ResizeBitmap;
  end;
end;

procedure TAnnotation.SetFontOrientation(Value: Integer);
begin
  if FFontOrientation <> Value then begin
    FFontOrientation := Value;
    FBitmap.FontOrientation := FFontOrientation;
    ResizeBitmap;
  end;
end;

procedure TAnnotation.SetFontColorString(Value: string);
begin
  if FFontColorString <> Value then begin
    FFontColorString := Value;
    Invalidate;
  end;
end;

procedure TAnnotation.ResizeBitmap;
var
  TextSize: TSize;
begin
  if Assigned(FBitmap) then begin
    TextSize := FBitmap.TextSize(FText);
    FWidth := TextSize.cx;
    FHeight := TextSize.cy;
    FBitmap.SetSize(FWidth, FHeight);
    Invalidate;
  end;
end;

procedure TAnnotation.SetBackgroundColorString(Value: string);
begin
  if FBackgroundColorString <> Value then begin
    FBackgroundColorString := Value;
    Invalidate;
  end;
end;

procedure TAnnotation.SetOwnerFilename(Value: string);
begin
  if FOwnerFilename <> Value then begin
    FOwnerFilename := Value;
    Invalidate;
  end;
end;

procedure TAnnotation.SetShape(Value: TTextGraphicShape);
begin
  if FShape <> Value then begin
    FShape := Value;
    Invalidate;
  end;
end;

// The AnnotationType property is for future use in filtering different annotation types
procedure TAnnotation.SetAnnotationType(Value: string);
begin
  if FAnnotationType <> Value then begin
    FAnnotationType := Value;
  end;
end;

procedure TAnnotation.SetBitmap(Value: TBGRABitmap);
begin
  if FBitmap <> Value then begin
    FBitmap := Value;
  end;
end;

constructor TAnnotation.Create;
begin
  inherited;
  FX := 0;
  FY := 0;
  FBitmap := TBGRABitmap.Create;
end;

destructor TAnnotation.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TAnnotation.PaintBackground(Shape: TTextGraphicShape);
var
  px: TBGRAPixel;
begin
  if (FBackgroundColor <> clNone) and (FShape <> shNone) then begin
    px := StrToBGRA(FBackgroundColorString);
    case FShape of
      shRectangle: MakeShape(FBitmap, shRectangle, FText, px);
      shRoundRect: MakeShape(FBitmap, shRoundRect, FText, px);
      shEllipse: MakeShape(FBitmap, shEllipse, FText, px);
      shCircle: MakeShape(FBitmap, shCircle, FText, px);
      shCloud: MakeShape(FBitmap, shCloud, FText, px);
      shScroll: MakeShape(FBitmap, shScroll, FText, px);
    end;
  end;
end;

// Repaint the annotation bitmap when necessary...
procedure TAnnotation.Invalidate;
var
  TextSz, ExpTextSize: TSize;
begin
  if (FText <> '') and Assigned(FBitmap) then begin
    TextSz := FBitmap.TextSize(FText);
    // Make the bitmap a bit wider than the text...
    ExpTextSize.cx := Round(TextSz.cx + (TextSz.cx/10));
    FBitmap.SetSize(ExpTextSize.cx, TextSz.cy);
    // Draw the background shape in the chosen color
    MakeShape(FBitmap, FShape, FText, StrToBGRA(FBackgroundColorString));
    with FBitmap do begin
      // Centre the text horizontally and vertically
      TextOut(FBitmap.Width div 2, Round(Height/2 - TextSz.cy/2), FText,
        StrToBGRA(FFontColorString), taCenter);
    end;
    // Center the completed bitmap over the origin point (mouse position)
    // and save its top-left corner coordinates...
    FLeft := FX - FBitmap.Width div 2;
    FTop := FY - FBitmap.Height div 2;
  end;
end;

end.

