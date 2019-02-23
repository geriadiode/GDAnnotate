unit AnnotationParamsDialogUnit;

{ This modal dialog facilitates creating and editing annotations.  Preview      }
{ components show the actual appearance of an annotation during creation and    }
{ any subsequent editing.                                                       }

{ Note that a unique ID is automatically assigned to each annotation at the     }
{ database level so we don't reference AnnotationID here.  We also ignore the   }
{ X and Y coordinate of an annotation as they are set from the View according   }
{ to mouse position. All color parameters are handled as strings representing   }
{ BGRAPixel colors.  This is necessary as Lazarus cannot (yet) generate RTTI for}
{ record types and mORMot relies on RTTI.  (TBGRAPizel is a record type.)       }

{ CopyLeft Geriadiode, 12th Feb 2019.                                           }
{ licence: Open Source LGPL/GPL.                                                }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Types,
  ComCtrls, BGRAVirtualScreen, BCListBox, BGRABitmap, BGRABitmapTypes, UtilityUnit;

type

  TDialogMode = (dmUnDefined, dmAdding, dmEditing);

  { TAnnotationParamsDialog }

  TAnnotationParamsDialog = class(TForm)
    bvsFont: TBGRAVirtualScreen;
    bvsBackground: TBGRAVirtualScreen;
    bvsPreview: TBGRAVirtualScreen;
    btnOK: TButton;
    btnCancel: TButton;
    btnFont: TButton;
    coldlgFont: TColorDialog;
    cbxShape: TComboBox;
    coldlgBack: TColorDialog;
    edtText: TEdit;
    FontDialog1: TFontDialog;
    gbxColors: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    tbFontTransparency: TTrackBar;
    tbBackTransparency: TTrackBar;
    procedure btnFontClick(Sender: TObject);
    procedure bvsBackgroundClick(Sender: TObject);
    procedure bvsBackgroundRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure bvsFontClick(Sender: TObject);
    procedure bvsFontRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure bvsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure cbxShapeChange(Sender: TObject);
    procedure edtTextChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure tbBackTransparencyChange(Sender: TObject);
    procedure tbFontTransparencyChange(Sender: TObject);
  private
    FDialogMode: TDialogMode;
    FText: string;
    FFontName: string;
    FFontHeight: Integer;
    FFontAntialias: Boolean;
    FFontStyle: TFontStyles;
    FFontPitch: TFontPitch;
    FFontOrientation: Integer;
    FFontBGRAPixel: TBGRAPixel;
    FFontColorString: string;
    FBackgroundBGRAPixel: TBGRAPixel;
    FBackgroundColorString: string;
    FShape: TTextGraphicShape;
    procedure SetFontColorString(Value: string);
    procedure SetBackgroundColorString(Value: string);
    procedure SetText(Value: string);
    procedure SetFontName(Value: string);
    procedure SetFontHeight(Value: Integer);
    procedure SetFontAntiAlias(Value: Boolean);
    procedure SetFontStyle(Value: TFontStyles);
    procedure SetFontPitch(Value: TFontPitch);
    procedure SetFontOrientation(Value: Integer);
    procedure SetShape(Value: TTextGraphicShape);
    procedure DrawPreview(var Bitmap: TBGRABitmap);
  public
    property DialogMode: TDialogMode read FDialogMode write FDialogMode;
    property Text: string read FText write SetText;
    property FontName: string read FFontName write SetFontName;
    property FontHeight: Integer read FFontHeight write FFontHeight;
    property FontAntialias: Boolean read FFontAntiAlias write SetFontAntiAlias;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property FontPitch: TFontPitch read FFontPitch write SetFontPitch;
    property FontOrientation: Integer read FFontOrientation write SetFontOrientation;
    property FontColorString: string read FFontColorString write SetFontColorString;
    property BackgroundColorString: string read FBackgroundColorString write SetBackgroundColorString;
    property Shape: TTextGraphicShape read FShape write SetShape;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  AnnotationParamsDialog: TAnnotationParamsDialog;

implementation

{$R *.lfm}

{ TAnnotationParamsDialog }

constructor TAnnotationParamsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  case FDialogMode of
    dmAdding: begin
      //Set defaults
      FText := '';
      FFontName := 'Times New Roman';
      FFontHeight := 12;
      FFontAntialias := True;
      FFontStyle := [];
      FFontPitch := fpDefault;
      FFontOrientation := 0;
      FFontColorString := BGRAToStr(BGRABlack);
      FShape := shRectangle;
    end;
  end;
end;

destructor TAnnotationParamsDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TAnnotationParamsDialog.SetFontColorString(Value: string);
begin
  if FFontColorString <> Value then begin
    FFontColorString := Value;
    FFontBGRAPixel := StrToBGRA(Value);
    FontDialog1.Font.Color := BGRAToColor(FFontBGRAPixel);
    tbFontTransparency.Position := FFontBGRAPixel.Alpha;
    bvsFont.RedrawBitmap;
  end;
end;

procedure TAnnotationParamsDialog.SetBackgroundColorString(Value: string);
begin
  if FBackgroundColorString <> Value then begin
    FBackgroundColorString := Value;
    FBackgroundBGRAPixel := StrToBGRA(Value);
    tbBackTransparency.Position := FBackgroundBGRAPixel.Alpha;
    bvsBackground.RedrawBitmap;
  end;
end;

procedure TAnnotationParamsDialog.btnFontClick(Sender: TObject);
begin
  if FontDialog1.Execute then begin
    FFontName := FontDialog1.Font.Name;
    FFontHeight := Abs(FontDialog1.Font.Height);
    FFontPitch := FontDialog1.Font.Pitch;
    FFontStyle := FontDialog1.Font.Style;
    FFontColorString := ColorToString(FontDialog1.Font.Color);
    FFontAntiAlias := True;
    FFontOrientation := 0;
    bvsFont.RedrawBitmap;
  end;
end;

procedure TAnnotationParamsDialog.bvsBackgroundClick(Sender: TObject);
begin
  if coldlgBack.Execute then begin
    FBackgroundBGRAPixel := ColorToBGRA(coldlgBack.Color, 255);
    tbBackTransparency.Position := 255;
    FBackgroundColorString := BGRAToStr(FBackgroundBGRAPixel);
    bvsBackground.ReDrawBitmap;
    bvsPreview.RedrawBitmap;
  end;
end;

// Repaint the background color preview
procedure TAnnotationParamsDialog.bvsBackgroundRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  Bitmap.FillRect(TBGRAVirtualScreen(Sender).ClientRect, FBackgroundBGRAPixel, dmDrawWithTransparency);
  bvsPreview.RedrawBitmap;
end;

procedure TAnnotationParamsDialog.bvsFontClick(Sender: TObject);
begin
  if coldlgFont.Execute then begin
    FFontBGRAPixel := ColorToBGRA(coldlgFont.Color, 255);
    tbBackTransparency.Position := 255;
    FFontColorString := BGRAToStr(FFontBGRAPixel);
    bvsFont.ReDrawBitmap;
  end;
end;

// Repaint the font color preview
procedure TAnnotationParamsDialog.bvsFontRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  Bitmap.FillRect(TBGRAVirtualScreen(Sender).ClientRect, FFontBGRAPixel, dmDrawWithTransparency);
  bvsPreview.RedrawBitmap;
end;

// Repaint the annotation preview
procedure TAnnotationParamsDialog.bvsPreviewRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
var
  TmpBitmap: TBGRABitmap;
begin
  TmpBitmap := TBGRABitmap.Create(bvsPreview.ClientWidth, bvsPreview.ClientHeight, clWhite);
  DrawPreview(TmpBitmap);
  // Need BGRABitmap pack 9.9.3 or greater for the following statement - which adjusts
  // the preview scale to maintain the correct aspect ratio...
  Bitmap.StretchPutImageProportionally(Bitmap.ClipRect, taCenter, tlCenter,
    TmpBitmap, dmDrawWithTransparency, 255);
  TmpBitmap.Free;
end;

// This procedure drawins a complete annotation for preview
procedure TAnnotationParamsDialog.DrawPreview(var Bitmap: TBGRABitmap);
var
  TextSize: TSize;
  X, Y: Integer;
begin
  Bitmap.FontHeight := bvsFont.ClientHeight;
  Bitmap.FillRect(Bitmap.ClipRect, BGRAPixelTransparent);
  TextSize := Bitmap.TextSize(FText);
  case FShape of
    shRectangle: MakeShape(Bitmap, shRectangle, FText, FBackgroundBGRAPixel);
    shRoundRect: MakeShape(Bitmap, shRoundRect, FText, FBackgroundBGRAPixel);
    shCircle: MakeShape(Bitmap, shCircle, FText, FBackgroundBGRAPixel);
    shEllipse: MakeShape(Bitmap, shEllipse, FText, FBackgroundBGRAPixel);
    shCloud: MakeShape(Bitmap, shCloud, FText, FBackgroundBGRAPixel);
    shScroll: MakeShape(Bitmap, shScroll, FText, FBackgroundBGRAPixel);
  end;
  // Center text in the bitmap...
  X := (Bitmap.Width div 2) - (TextSize.cx div 2);
  Y := (Bitmap.Height div 2) - (TextSize.cy div 2);
  Bitmap.TextOut(X, Y, FText, FFontBGRAPixel, taLeftJustify);
end;

// As selection of background shapes
procedure TAnnotationParamsDialog.cbxShapeChange(Sender: TObject);
begin
  case TComboBox(Sender).ItemIndex of
    0: FShape := shNone;
    1: FShape := shRectangle;
    2: FShape := shRoundRect;
    3: FShape := shCircle;
    4: FShape := shEllipse;
    // THe following shapes come from png or jpg resources
    5: FShape := shCloud;
    6: FShape := shScroll;
  end;
  bvsPreview.RedrawBitmap;
end;

procedure TAnnotationParamsDialog.edtTextChange(Sender: TObject);
begin
  FText := TEdit(Sender).Text;
  bvsPreview.RedrawBitmap;
end;

procedure TAnnotationParamsDialog.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  FText := edtText.Text;
  FFontColorString := BGRAToStr(FFontBGRAPixel);
  FBackgroundColorString := BGRAToStr(FBackgroundBGRAPixel);
  CanClose := True;
end;

procedure TAnnotationParamsDialog.tbBackTransparencyChange(Sender: TObject);
begin
  FBackGroundBGRAPixel.Alpha := TTrackBar(Sender).Position;
  bvsBackground.RedrawBitmap;
end;

procedure TAnnotationParamsDialog.tbFontTransparencyChange(Sender: TObject);
begin
  FFontBGRAPixel.Alpha := TTrackBar(Sender).Position;
  bvsFont.RedrawBitmap;
end;

procedure TAnnotationParamsDialog.SetText(Value: string);
begin
  FText := Value;
  edtText.Text := Value;
end;

procedure TAnnotationParamsDialog.SetFontName(Value: string);
begin
  if FFontName <> Value then begin
    FFontName := Value;
    FontDialog1.Font.Name := Value;
  end;
end;

procedure TAnnotationParamsDialog.SetFontHeight(Value: Integer);
begin
  if FFontHeight <> Value then begin
    FFontHeight := Value;
    FontDialog1.Font.Height := Value;
  end;
end;

procedure TAnnotationParamsDialog.SetFontAntiAlias(Value: Boolean);
begin
  if FFontAntiAlias <> Value then
    FFontAntiAlias := Value;
end;

procedure TAnnotationParamsDialog.SetFontStyle(Value: TFontStyles);
begin
  if FFontStyle <> Value then begin
    FFontStyle:= Value;
    FontDialog1.Font.Style := Value;
  end;
end;

procedure TAnnotationParamsDialog.SetFontPitch(Value: TFontPitch);
begin
  if FFontPitch <> Value then begin
    FFontPitch := Value;
    FontDialog1.Font.Pitch := Value;
  end;
end;

procedure TAnnotationParamsDialog.SetFontOrientation(Value: Integer);
begin
  if FFontOrientation <> Value then begin
    FFontOrientation := Value;
    FontDialog1.Font.Orientation := Value;
  end;
end;

procedure TAnnotationParamsDialog.SetShape(Value: TTextGraphicShape);
begin
  FShape := Value;
  cbxShape.ItemIndex := Ord(Value);
end;

end.

