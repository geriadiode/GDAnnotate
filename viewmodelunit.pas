unit ViewModelUnit;

{$mode objfpc}{$H+}

interface

uses
  {$I Synopse.inc}
  Classes, SysUtils, Forms, Controls, Graphics, Types, contnrs, SynCommons,
  Dialogs, lclproc, mORMot, SynSQLite3, SynSQLite3Static, mORMotSQLite3,
  BGRABitmap, BGRABitmapTypes, ModelUnit, AnnotationParamsDialogUnit;

type

  { TPersist is the 'ViewModel' part (VM) of the MVVM design pattern.            }
  { It handles two-day communication between the View which displays annotations,}
  { and the Model which defines an annotation's properties and methods. It also  }
  { contains a procedure that calls on mORMot to automatically create an SQLite3 }
  { database needed for object persistence plus a related Client and Server.     }
  { Note that creating these items takes only a few lines of code.  There is no  }
  { need for any conventional database design process, and no need to write any  }
  { SQL statements!                                                              }

  { The database uses the usual CRUD paradigm (Create, Retrieve, Update, Delete).}

  { CopyLeft Geriadiode, 12th Feb 2019.                                          }
  { Licence. Open Source LGPL/GPL                                                }

  {------------------------------------------------------------------------------}
  { The following interface represents all of the ViewModel as seen by the View. }
  { An interface is not mandatory - but it is good practice.                     }
  { Annotation Creates, Retrieves, Updates, or Deletes are flagged to the View   }
  { unit via a single TNotifyEvent whose event procedure is set via the          }
  { SetEventProc procedure.                                                      }

  IPersist = interface['{D6EAADE2-B002-4A8C-A858-F955E0A9EDBE}']
    function NewAnnotationAt(Point: TPoint; Filename: string): Boolean;
    function GetAnnotationByID(ID: TID): TAnnotation;
    function GetAllAnnotations: TObjectList;
    function GetAnnotationsForFilename(Filename: string): TObjectList;
    function DeleteAnnotationByID(ID: TID): Boolean;
    function DeleteAllAnnotations: Boolean;
    function DeleteAllAnnotationsForFilename(Filename: string): Boolean;
    function EditAnnotation(Annotation: TAnnotation): Boolean;
    function MoveAnnotation(Annotation: TAnnotation; Point: TPoint): Boolean;
    function GetAnnotationUnderMouse(MousePoint: TPoint): TAnnotation;
    procedure SetEventProc(Proc: TNotifyEvent);
  end;

  TPersist = class(TInterfacedObject, IPersist)
  private
    FRestServer: TSQLRestServerDB;
    FRestClient: TSQLRestClientURI;
    FOnModelUpdate: TnotifyEvent;
    FEventProc: TNotifyEvent;
    FAnnotationList: TObjectList;
    procedure SetOnModelUpdate(Value: TNotifyEvent);
    function ModelUpdated(Sender: TSQLRestServer; Event: TSQLEvent; Table:
        TSQLRecordClass; const ID: TID; const aSentData: RawUTF8): Boolean;
  public
    property OnModelUpdate: TNotifyEvent read FOnModelUpdate write SetOnModelUpdate;
    function NewAnnotationAt(Point: TPoint; Filename: string): Boolean;
    function GetAnnotationByID(ID: TID): TAnnotation;
    function GetAllAnnotations: TObjectList;
    function GetAnnotationsForFilename(Filename: string): TObjectList;
    function DeleteAnnotationByID(ID: TID): Boolean;
    function DeleteAllAnnotations: Boolean;
    function DeleteAllAnnotationsForFilename(Filename: string): Boolean;
    function EditAnnotation(Annotation: TAnnotation): Boolean;
    function MoveAnnotation(Annotation: TAnnotation; Point: TPoint): Boolean;
    function GetAnnotationUnderMouse(MousePoint: TPoint): TAnnotation;
    procedure SetEventProc(Proc: TNotifyEvent);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TPersist.Create;
begin
  inherited;
  // This code triggers automatic creation of the required SQLite3 database
  // with tables and fields that match the Model (in ModelUnit.pas).
  // We also setup the client and server components here.
  FRestServer := TSQLRestServerDB.CreateWithOwnModel(
                   [ModelUnit.TAnnotation], 'Annotation.DB' );
  FRestServer.ExportServer;
  FRestServer.CreateMissingTables;
  FRestServer.OnUpdateEvent := @ModelUpdated;
  FRestClient := TSQLRestClientURIDll.Create(
                      TSQLModel.Create(FRestServer.Model),
                      @URIRequest);
  FRestClient.Model.Owner := FRestClient;
end;

destructor TPersist.Destroy;
begin
  FAnnotationList.Free;
  FRestServer.Free;
  FRestClient.Free;
end;

// Advise the ViewModel re which View procedure to call whenever a Model object changes
// on the Server side.
procedure TPersist.SetOnModelUpdate(Value: TNotifyEvent);
begin
  FOnModelUpdate := Value;
end;

// All changes in the Model that happen at the server get signalled here...
function TPersist.ModelUpdated(Sender: TSQLRestServer; Event: TSQLEvent; Table:
    TSQLRecordClass; const ID: TID; const aSentData: RawUTF8): Boolean;
begin
  // Here we signal the View that a change has occured.  We are unconcerned with the type of change.
  if Assigned(FOnModelUpdate) then
    FOnModelUpdate(Self);
  Result := True;
end;

// Create a new annotation object...
function TPersist.NewAnnotationAt(Point: TPoint; Filename: string): Boolean;
var
  Annotation: ModelUnit.TAnnotation;
  Dlg: TAnnotationParamsDialog;
begin
  Annotation := ModelUnit.TAnnotation.Create;
  with Annotation do begin
    x := Point.x;
    y := Point.y;
    Dlg := TAnnotationParamsDialog.Create(Nil);
    try
      Dlg.ShowModal;
      if Dlg.ModalResult = mrOK then begin
        Text := Dlg.Text;
        FontName := Dlg.FontName;
        FontHeight := Dlg.FontHeight;
        FontAntiAlias := Dlg.FontAntiAlias;
        FontStyle := Dlg.FontStyle;
        FontOrientation := Dlg.FontOrientation;
        FontColorString := Dlg.FontColorString;
        BackgroundColorString := Dlg.BackGroundColorString;
        Shape := Dlg.Shape;
        AnnotationType := 'Annotation';
        OwnerFilename := Filename;
      end;
      // This line creates a new database record...
      FRestServer.Add(Annotation, true);
    finally
      Dlg.Free;
    end;
  end;
  Result := True;
  // Ensure that the View is advised of the change...
  if Assigned(FOnModelUpdate) then
    FOnModelUpdate(Self);
end;

// Retrieve all annotation objects from the database as a TObjectList
function TPersist.GetAllAnnotations: TObjectList;
begin
  Result := FRestServer.RetrieveList(TAnnotation, '1=?', [1]);
  // Note: '1=?, [1] is equivalent to 'WHERE 1=1' in an SQL statement.
  //       'WHERE 1=1' means 'fetch all records'.
  FAnnotationList := Result;
end;

// Retrieve all annotation objects by owner filename from the database as a TObjectList
function TPersist.GetAnnotationsForFilename(Filename: string): TObjectList;
begin
  Result := FRestServer.RetrieveList(TAnnotation, 'OwnerFilename=?', [Filename]);
  // Equivalent to SQL: 'select * from annotations where OwnerFilename = ' + Filename;
end;

// Fetch one annotation object by its unique ID.
function TPersist.GetAnnotationByID(ID: TID): TAnnotation;
begin
  Result := TAnnotation.Create(FRestClient, ID);
  // Note: We need to create a new instance because the MODEL defines CLASSES, not OBJECTS
end;

// Clear out all annotations (with user confirmation).
function TPersist.DeleteAllAnnotations: Boolean;
var
  List: TObjectList;
  i: Integer;
  ID: TID;
begin
  if MessageDlg('Delete ALL annotations. Are You Sure?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then begin
      Result := True;
    try
      // Delete all annotations fron the server (clears the database table)
      List := GetAllAnnotations;
      for i := 0 to List.Count-1 do begin
        ID := TAnnotation(List.Items[i]).ID;
        FRestServer.Delete(TAnnotation, ID);
      end;
    except
      Result := False;
    end;
  end;
  // Signal the View to clear the screen...
  if Assigned(FOnModelUpdate) then
    FOnModelUpdate(Self);
end;

function TPersist.DeleteAllAnnotationsForFilename(Filename: string): Boolean;
var
  List: TObjectList;
  i: Integer;
  ID: TID;
begin
  if MessageDlg('Delete ALL annotations for this document. Are You Sure?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then begin
      Result := True;
    try
      // Delete all annotations fron the server (clears the database table)
      List := GetAnnotationsForFilename(Filename);
      for i := 0 to List.Count-1 do begin
        ID := TAnnotation(List.Items[i]).ID;
        FRestServer.Delete(TAnnotation, ID);
      end;
    except
      Result := False;
    end;
  end;
  // Signal the View to clear the screen...
  if Assigned(FOnModelUpdate) then
    FOnModelUpdate(Self);
end;

// Delete a single annotation by ID.
function TPersist.DeleteAnnotationByID(ID: TID): Boolean;
begin
  Result := True;
  try
    // Delete from server
    FRestServer.Delete(TAnnotation, ID);
  except
    Result := False;
  end;
  // Delete from View screen
  if Assigned(FOnModelUpdate) then
    FOnModelUpdate(Self);
end;

// Edit a previously-selected annotation object using a dedicated edit dialog.
function TPersist.EditAnnotation(Annotation: TAnnotation): Boolean;
var
  Dlg: TAnnotationParamsDialog;
  An: ModelUnit.TAnnotation;
begin
  Result := True;
  if Assigned(Annotation) then begin
    try
      Dlg := TAnnotationParamsDialog.Create(Nil);
      try
        Dlg.Text := Annotation.Text; // Original annotation to dialog
        Dlg.FontName := Annotation.FontName;
        Dlg.FontHeight := Annotation.FontHeight;
        Dlg.FontDialog1.Font.Size := Dlg.FontHeight;
        Dlg.FontAntiAlias := Annotation.FontAntiAlias;
        Dlg.FontStyle := Annotation.FontStyle;
        Dlg.FontOrientation := Annotation.FontOrientation;
        Dlg.FontColorString := Annotation.FontColorString;
        Dlg.BackGroundColorString := Annotation.BackGroundColorString;
        Dlg.Shape:= Annotation.Shape;
        Dlg.ShowModal;
        if Dlg.ModalResult = mrOK then begin
          try
            An := ModelUnit.TAnnotation.Create(FRestServer, Annotation.ID); // Replacement annotation from dialog
            An.Text := Dlg.Text;
            An.FontName := Dlg.FontName;
            An.FontHeight := Dlg.FontHeight;
            An.FontAntiAlias := Dlg.FontAntiAlias;
            An.FontStyle := Dlg.FontStyle;
            An.FontOrientation := Dlg.FontOrientation;
            An.FontColorString := Dlg.FontColorString;
            An.BackgroundColorString := Dlg.BackgroundColorString;
            An.Shape := Dlg.Shape;
            FRestServer.Update(An);
            if Assigned(FOnModelUpdate) then
              FOnModelUpdate(Self);
          finally
            An.Free;
          end;
        end;
      finally
        Dlg.Free;
      end;
    except
      Result := False;
    end;
  end;
end;

// Move an annotation simply by changing its coordinates on the server.
function TPersist.MoveAnnotation(Annotation: TAnnotation; Point: TPoint): Boolean;
var
  An: TAnnotation;
begin
  Result := True;
  if Assigned(Annotation) then begin
    try
      An := ModelUnit.TAnnotation.Create(FRestServer, Annotation.ID); // Replacement item
      An.X := Point.X;
      An.Y := Point.Y;
      FRestServer.Update(An);
      if Assigned(FOnModelUpdate) then
        FOnModelUpdate(Self);
    except
      Result := False;
    end;
  end;
end;

// Set the View procedure that needs to be called whenever a Model object changes
// on the Server.
procedure TPersist.SetEventProc(Proc: TNotifyEvent);
begin
  FEventProc := Proc;
  FOnModelUpdate := FEventProc;
end;

// This function returns the annotation currently under the mouse coordinates.
// Returns Nil if the mouse is not currently over an annotation.
function TPersist.GetAnnotationUnderMouse(MousePoint: TPoint): TAnnotation;
var
  i: Integer;
  An: TAnnotation;
  Rect: TRect;
begin
  Result := Nil;
  FAnnotationList := GetAllAnnotations;
  for i := 0 to FAnnotationList.Count-1 do begin
    An := TAnnotation(FAnnotationList.Items[i]);
    Rect := An.Bitmap.ClipRect;
    MoveRect(Rect, An.Left, An.Top);
    if PtInRect(Rect, MousePoint) then begin
      Result := An;
      break;
    end;
  end;
end;

end.

