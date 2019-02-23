unit viewunit_MAIN;  //  THIS IS THE MAIN FORM.

{ PROGRAM: GDANNOTATE:                                                          }

{ This NewPascal (Lazarus) Windows-only demo application is a fragment of a     }
{ much larger program that targets keyboard music practice. The code presented  }
{ here was developed as a feasibility exercise to determine the                 }
{ advantages/disadvantages of re-writing the original Delphi-RAD version as     }
{ a mORMot MVC/MVVM equivalent.  The current fragment is being offered now in   }
{ hope that it may be useful as an example to other NewPascal/Lazarus           }
{ developers who need help in getting started with the mORMot framework.        }

{ Results of the feasibility study suggest that a complete re-write of the old  }
{ RAD program is fully justified to achieve at least equal performance  with    |
{ fewer lines of code having much better structure that is easier to read and   }
{ maintain.  The new strategy also drives good separation of the                }
{ 'business logic' from the presentation layer - which was impossible to        }
{ achieve in the original RAD design.                                           }

{-------------------------------------------------------------------------------}
{ USER STORY: GDANNOTATE:                                                       }

{ Sheet music intended for practice is often annotated by a performer to sjow   }
{   performance hints such as chosen fingerings, instrument changes, and        }
{   dynamics, etc. In this program the score is displayed on a PC screen rather }
{   than on a printed page.  (IPads displaying PDFs now commonly replace        }
{   printed scores in performance sessions).  This app is a PC-only version     }
{   that could run on a Windows laptop.                                         }

{ The ACTOR is a user seated at a music keyboard with an associated PC screen.  }

{ The main program window can load and display a document such as a page of     }
{ sheet music.  This window has a transparent overlay that can accept           }
{ annotations that are text objects on a background shape such as a rectangle   }
{ or circle, etc.                                                               }

{ The sctor uses the PC keyboard and mouse to add annotations to the score.     }

{ A right mouse button click, not over an existing annotation, pops up a menu to}
{   allow a new annotation to be created at the mouse position. A popup dialog  }
{   allows customization of the annotation's text, font and background shape.   }
{   The text and  background shape have a choice of colors, and both may be set }
{   to any opacity between fully opaque and fully transparent.                  }
{   The same menu also caters for clearing all existing annotations.            }

{ A right mouse button click over an existing annotation popus up a menu to     }
{  allow that annotation to be edited or deleted.                               }

{ Whenever the mouse cursor passes over an existing annotation, that annotation }
{  is highlighed with a focus rectangle.                                        }

{ A left mouse button drag allows the selected annotation to be repositioned    }
{   anywhere on the score.                                                      }

{ The relative positions of all annotations in relation to the underlying score }
{  are maintained if the user resizes the display window.                       }

{ All annotations are PERSISTENT between sessions due to the mORMot framework   }
{  working in Client-Server mode in associated with an automatically-generated  }
{  SQLite3 database.                                                            }

{ UNIT STRUCTURE GDANNOTATE:
}
{ ViewUnit_MAIN.pas (This unit) - contains only GUI items (VIEW).               }
{ ModelUnit.pas - the MODEL which holds object classes ('business logic').      }
{ ViewModel.pas - the VIEW_MODEL module which faciliates two-way communication  }
{                 between the VIEW and the MODEL.                               }
{ AnnotationParamsDialogUnit.pas - a dialog to facilitate creating and editing  }
{                 annotations.                                                  }
{ UtilityUnit.pas - contains common logic referenced both by the application    }
{                 and the AnnotationParamsDialog.                               }

{ COMPILING GDANNOTATE:                                                         }
{   Needs NewPascal oR Lazarus version 1.9.0 or later.                          }
{   mORMot (Install using FPCUpDeluxe.exe or else download from Github).        }
{   BGRABitmapPack 9.9.3 (Install using Lararus Online Package Manager).        }

{ CopyLeft Geriadiode, 23 Feb 2019.                                             }
{ Licence. Open Source LGPL/GPL                                                 }
{-------------------------------------------------------------------------------}

{$mode objfpc}{$H+}

// This resource contains a few pre-defined annotation background shapes (png or jpg).
{$R Shapes.rc}

interface

uses
  {$I Synopse.inc} // Essential to mORMot
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList,
  Types, Contnrs, Menus, ComCtrls, lclproc, BGRABitmap, BGRABitmapTypes, BGRALayers,
  BGRAVirtualScreen, ModelUnit, ViewModelUnit;

const
  // Load a default document at startup
  DEFAULT_DOCUMENT_PATH = 'Cantabile-1.jpg';

type

  { TViewForm }

  TViewForm = class(TForm)
    PurgeDatabaseAction: TAction;
    LoadDocumentAction: TAction;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    ClearAnnotationsAction: TAction;
    DeleteAnnotationAction: TAction;
    EditAnnotationAction: TAction;
    ItemAdd: TMenuItem;
    ItemClear: TMenuItem;
    ItemDelete: TMenuItem;
    ItemEdit: TMenuItem;
    MainActionList: TActionList;
    MainMenu1: TMainMenu;
    mnuPurge: TMenuItem;
    mnuDebug: TMenuItem;
    mnuEditOrDelete: TPopupMenu;
    mnuFile: TMenuItem;
    mnuLoadTextDoc: TMenuItem;
    NewAnnotationAction: TAction;
    OpenDialog1: TOpenDialog;
    RightClickPopupMenu: TPopupMenu;
    SetupPersistenceAction: TAction;
    StatusBar1: TStatusBar;
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAVirtualScreen1Resize(Sender: TObject);
    procedure ClearAnnotationsActionExecute(Sender: TObject);
    procedure DeleteAnnotationActionExecute(Sender: TObject);
    procedure EditAnnotationActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ItemAddClick(Sender: TObject);
    procedure ItemClearClick(Sender: TObject);
    procedure ItemDeleteClick(Sender: TObject);
    procedure ItemEditClick(Sender: TObject);
    procedure LoadDocumentActionExecute(Sender: TObject);
    procedure mnuLoadTextDocClick(Sender: TObject);
    procedure NewAnnotationActionExecute(Sender: TObject);
    procedure PurgeDatabaseActionExecute(Sender: TObject);
    procedure SetupPersistenceActionExecute(Sender: TObject);
  private
    FFilename: string;
    FLayers: TBGRALayeredBitmap;
    FPicBitmap: TBGRABitmap;
    FBaseLayer: TBGRABitmap;
    FAnnotationLayer: TBGRABitmap;
    FFocusLayer: TBGRABitmap;
    FSelectedAnnotation: TAnnotation;
    FPersistIntf: IPersist;
    FDragging: Boolean;
    FOriginalDocSize: TSize;
    HScale: Extended;
    VScale: Extended;
    procedure PopulateFocusLayer(FocusedAnnotation: TAnnotation);
    procedure ClearFocusLayer;
    procedure ModelUpdated(Sender: TObject);
  public

  end;

var
  ViewForm: TViewForm;

implementation

{$R *.lfm}

{ TViewForm }

procedure TViewForm.FormCreate(Sender: TObject);
begin
  FOriginalDocSize.cx := BGRAVirtualScreen1.ClientWidth;
  FOriginalDocSize.cy := BGRAVirtualScreen1.ClientHeight;
  HScale := 1;
  VScale := 1;
  FFileName := DEFAULT_DOCUMENT_PATH;
  // The main window uses a layered bitmap
  FLayers := TBGRALayeredBitmap.Create(BGRAVirtualScreen1.ClientWidth, BGRAVirtualScreen1.ClientHeight);
  // Base layer for the document
  FPicBitmap := TBGRABitmap.Create(FFileName);
  FBaseLayer := TBGRABitmap.Create(BGRAVirtualScreen1.ClientWidth, BGRAVirtualScreen1.ClientHeight, clWhite);
  FBaseLayer.StretchPutImage(FBaseLayer.ClipRect, FPicBitmap, dmDrawWithTransparency, 255);
  // Transparent overlay for annotations
  FAnnotationLayer := TBGRABitmap.Create(BGRAVirtualScreen1.ClientWidth, BGRAVirtualScreen1.ClientHeight);
  // The focus rectangle has its own layer
  FFocusLayer := TBGRABitmap.Create(BGRAVirtualScreen1.ClientWidth, BGRAVirtualScreen1.ClientHeight);
  FLayers.AddOwnedLayer(FBaseLayer);
  FLayers.AddOwnedLayer(FAnnotationLayer);
  FLayers.AddOwnedLayer(FFocusLayer);
  // This starts the persistence framework...
  SetupPersistenceAction.Execute;
end;

procedure TViewForm.FormDestroy(Sender: TObject);
begin
  FLayers.Free;
end;

procedure TViewForm.ItemAddClick(Sender: TObject);
begin
  NewAnnotationAction.Execute;
end;

procedure TViewForm.ItemClearClick(Sender: TObject);
begin
  ClearAnnotationsAction.Execute;
end;

procedure TViewForm.ItemDeleteClick(Sender: TObject);
begin
  DeleteAnnotationAction.Execute;
end;

procedure TViewForm.ItemEditClick(Sender: TObject);
begin
  EditAnnotationAction.Execute;
end;

// Load a new document (png or jpg) - does not cater for pdf yet.
procedure TViewForm.LoadDocumentActionExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    FFileName := OpenDialog1.FileName;
    FPicBitmap.LoadFromFile(FFileName);
    FBaseLayer.FillRect(FBaseLayer.ClipRect, clWhite);
    FBaseLayer.StretchPutImage(FBaseLayer.ClipRect, FPicBitmap, dmDrawWithTransparency, 255);
    BGRAVirtualScreen1.RedrawBitmap;
  end else
    ShowMessage('Failed to load requested file');
end;

procedure TViewForm.mnuLoadTextDocClick(Sender: TObject);
begin
  LoadDocumentAction.Execute;
end;

procedure TViewForm.NewAnnotationActionExecute(Sender: TObject);
var
  pt: TPoint;
begin
  pt := RightClickPopupMenu.PopupPoint;
  pt := ScreenToClient(Point(pt.x - BGRAVirtualScreen1.Left, pt.y - BGRAVirtualScreen1.Top));
  FPersistIntf.NewAnnotationAt(pt, FFilename);
end;

procedure TViewForm.PurgeDatabaseActionExecute(Sender: TObject);
begin
  if MessageDlg('DELETE ALL ANNOTATIONS ACROSS ALL DOCUMENTS? - ARE YOU SURE?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      FPersistIntf.DeleteAllAnnotations;
end;

procedure TViewForm.SetupPersistenceActionExecute(Sender: TObject);
begin
  FPersistIntf := TPersist.Create;
  FPersistIntf.SetEventProc(@ModelUpdated);
end;

procedure TViewForm.ModelUpdated(Sender: TObject);
begin
  BGRAVirtualScreen1.ReDrawBitmap;
end;

// This procedure repaints the entire display screen on any annotation change.
procedure TViewForm.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  An: TAnnotation;
  List: TObjectList;
  i: Integer;
  TmpBitmap: TBGRABitmap;
begin
  // Clear annotationlayer to transparent...
  FAnnotationLayer.FillRect(FAnnotationLayer.ClipRect, BGRAPixelTransparent, dmSet);
  // Refresh all annotations (relevant to the currently loaded document) from the server...
  List := FPersistIntf.GetAnnotationsForFilename(FFilename);
  // Copy each annotation to its correct position on the display...
  for i := 0 to List.Count-1 do begin
    An := TAnnotation(List.Items[i]);
     // Center each annotation on mouse X, Y
    FAnnotationLayer.PutImage(Round(HScale * An.Left), Round(VScale * An.Top), An.Bitmap, dmDrawWithTransparency);
  end;
  // TODO Additional layers for freehand drawing, highlighter pen, etc.

  // Combine all layers into a single bitmap...
  TmpBitmap := FLayers.ComputeFlatImage;
  // Scale to fit available screen area before displaying ...
  Bitmap.StretchPutImage(Bitmap.ClipRect, TmpBitmap, dmDrawWithTransparency);
  TmpBitmap.Free;
end;

procedure TViewForm.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ScreenPoint: TPoint;
  ScaledX, ScaledY: Integer;
begin
  ScreenPoint := ClientToScreen(Types.Point(BGRAVirtualScreen1.Left + X, BGRAVirtualScreen1.Top + Y));
  ScaledX := Round(X/HScale);
  ScaledY := Round(Y/VScale);
  FSelectedAnnotation := FPersistIntf.GetAnnotationUnderMouse(Point(ScaledX, ScaledY));
  if Assigned(FSelectedAnnotation) then begin  // If mouse is over an annotation
    case Button of
      mbLeft: if Assigned(FSelectedAnnotation) then
        FDragging := True;
      mbRight: if Assigned(FSelectedAnnotation) then
        mnuEditOrDelete.Popup(ScreenPoint.X, ScreenPoint.Y);
    end;
  end else begin // if mouse is not over an annotation
    case Button of
      mbRight: RightClickPopupMenu.PopUp(ScreenPoint.X, ScreenPoint.Y);
    end;
  end;
end;

procedure TViewForm.BGRAVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  FocusedAnnotation: TAnnotation;
  ScaledX, ScaledY: Integer;
begin
  ScaledX := Round(X/HScale);
  ScaledY := Round(Y/VScale);
  // Show the current mouse coordinates as a convenience feature ...
  StatusBar1.SimpleText := '';
  StatusBar1.SimpleText := 'X=' +IntToStr(X) + ', ' + 'Y=' + IntToStr(Y);
  // Get the currently focused annotation (or Nil)
  FocusedAnnotation := FPersistIntf.GetAnnotationUnderMouse(Point(ScaledX+1, ScaledY+1));
  if Assigned(FocusedAnnotation) then // Cursor is over an annotation
    PopulateFocusLayer(FocusedAnnotation)
  else // Cursor is not over an annotation.
    ClearFocusLayer;
  // Drag the annotation if we are over one and we move the mouse with the left button down.
  // This action completely replaces the old annotation on the server with a new version before
  // triggering a screen repaint.
  if Assigned(FSelectedAnnotation) and FDragging then
    FPersistIntf.MoveAnnotation(FSelectedAnnotation, Point(ScaledX, ScaledY));
end;

procedure TViewForm.BGRAVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
end;

procedure TViewForm.BGRAVirtualScreen1Resize(Sender: TObject);
begin
  HScale := BGRAVirtualScreen1.ClientWidth/FOriginalDocSize.cx;
  VScale := BGRAVirtualScreen1.ClientHeight/FOriginalDocSize.cy;
  FLayers.SetSize(BGRAVirtualScreen1.ClientWidth, BGRAVirtualScreen1.ClientHeight);
  FBaseLayer.SetSize(BGRAVirtualScreen1.ClientWidth, BGRAVirtualScreen1.ClientHeight);
  FBaseLayer.StretchPutImage(FBaseLayer.ClipRect, FPicBitmap, dmDrawWithTransparency, 255);
  FAnnotationLayer.SetSize(BGRAVirtualScreen1.ClientWidth, BGRAVirtualScreen1.ClientHeight);
  FFocusLayer.SetSize(BGRAVirtualScreen1.ClientWidth, BGRAVirtualScreen1.ClientHeight);
  BGRAVirtualScreen1.RedrawBitmap;
end;

// Clear only those annotations attached to the current document
procedure TViewForm.ClearAnnotationsActionExecute(Sender: TObject);
begin
  FPersistIntf.DeleteAllAnnotationsForFilename(FFilename);
end;

procedure TViewForm.DeleteAnnotationActionExecute(Sender: TObject);
begin
  FPersistIntf.DeleteAnnotationByID(FSelectedAnnotation.ID);
end;

procedure TViewForm.EditAnnotationActionExecute(Sender: TObject);
begin
  // This action launches an instance of AnnotationParamsDialog...
  if Assigned(FSelectedAnnotation) then
    FPersistIntf.EditAnnotation(FSelectedAnnotation);
end;

procedure TViewForm.PopulateFocusLayer(FocusedAnnotation: TAnnotation);
var
  Rect: TRect;
begin
  // Clear layer to transparent
  FFocusLayer.FillRect(FFocusLayer.ClipRect, BGRAPixelTransparent, dmSet);
  // Look for a currently focused annotation.  If there is one draw a red focus rectangle around it.
  //  Note that this action involves local GUI activity only and the database is not involved.
  if Assigned(FocusedAnnotation) then begin
    Rect := FocusedAnnotation.Bitmap.ClipRect;
    MoveRect(Rect, Round(HScale * FocusedAnnotation.Left), Round(VScale * FocusedAnnotation.Top));
    FFocusLayer.Rectangle(Rect, clRed,  dmdrawWithTransparency);
    BGRAVirtualScreen1.RedrawBitmap;
  end;
end;

// Clear the layer containing the focus rectangle whenever an annotation loses focus.
procedure TViewForm.ClearFocusLayer;
begin
  if Assigned(FFocusLayer) then begin
    FFocusLayer.Free;
    FFocusLayer := TBGRABitmap.Create(BGRAVirtualScreen1.ClientWidth, BGRAVirtualScreen1.ClientHeight);
    FFocusLayer.FillRect(FFocusLayer.ClipRect, BGRAPixelTransparent, dmdrawWithTransparency);
    BGRAVirtualScreen1.RedrawBitmap;
  end;
end;

end.

