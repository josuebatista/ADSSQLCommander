unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, DBTables, ADODB, ExtCtrls, Buttons;

type
  TADSform = class(TForm)
    DataSource: TDataSource;
    DBGrid1: TDBGrid;
    ExecuteBtn: TButton;
    ADOQuery: TADOQuery;
    ADOConnection1: TADOConnection;
    ADOTable: TADOTable;
    RadioGroup1: TRadioGroup;
    TableName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    TableIndex: TEdit;
    Label3: TLabel;
    TableFilter: TEdit;
    Label4: TLabel;
    ConnectStr: TMemo;
    SqlCommand: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    CloseAllBtn: TBitBtn;
    ExportCsvBtn: TButton;
    ExportSaveDialog: TSaveDialog;
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExportCsvBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ADSform: TADSform;

implementation

{$R *.dfm}

function CleanConnectionString(const MemoText: string): string;
begin
  // Remove ALL line breaks for connection strings (no spaces)
  Result := Trim(StringReplace(
    StringReplace(MemoText, #13, '', [rfReplaceAll]), 
    #10, '', [rfReplaceAll]
  ));
end;

function CleanSQLString(const MemoText: string): string;
begin
  // Replace line breaks with spaces for SQL statements
  Result := StringReplace(
    StringReplace(MemoText, #13#10, ' ', [rfReplaceAll]), 
    #10, ' ', [rfReplaceAll]
  );
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
  
  // Remove multiple consecutive spaces
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
  
  Result := Trim(Result);
end;


// ****************************************************
{ These functions provide both quick client-side validation and thorough database-level syntax checking.
  The Prepared := True statement will catch syntax errors without actually executing the query
}
// ****************************************************
function CountChar(const S: string; C: Char): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      Inc(Result);
end;

function ValidateSQLBasic(const SQLText: string): Boolean;
var
  CleanSQL: string;
  UpperSQL: string;
begin
  Result := False;
  CleanSQL := Trim(SQLText);
  
  if Length(CleanSQL) = 0 then
    Exit;
  
  UpperSQL := UpperCase(CleanSQL);
  
  if (Pos('SELECT', UpperSQL) > 0) or
     (Pos('INSERT', UpperSQL) > 0) or
     (Pos('UPDATE', UpperSQL) > 0) or
     (Pos('DELETE', UpperSQL) > 0) or
     (Pos('EXECUTE', UpperSQL) > 0) then
  begin
    if (CountChar(CleanSQL, '(') = CountChar(CleanSQL, ')')) and
       (CountChar(CleanSQL, '''') mod 2 = 0) then
      Result := True;
  end;
end;

function ValidateSQLSyntax(AQuery: TADOQuery; const SQLText: string; out ErrorMsg: string): Boolean;
var
  OriginalSQL: TStrings;
  OriginalActive: Boolean;
begin
  Result := False;
  ErrorMsg := '';
  
  // First do basic validation
  if not ValidateSQLBasic(SQLText) then
  begin
    ErrorMsg := 'Invalid SQL: Missing required keywords or unmatched brackets/quotes';
    Exit;
  end;
  
  OriginalSQL := TStringList.Create;
  try
    OriginalSQL.Assign(AQuery.SQL);
    OriginalActive := AQuery.Active;
    
    try
      AQuery.Active := False;
      AQuery.SQL.Clear;
      AQuery.SQL.Add(SQLText);
      AQuery.Prepared := True;
      Result := True;
      
    except
      on E: Exception do
      begin
        Result := False;
        ErrorMsg := 'SQL Syntax Error: ' + E.Message;
      end;
    end;
    
  finally
    AQuery.Prepared := False;
    AQuery.SQL.Assign(OriginalSQL);
    AQuery.Active := OriginalActive;
    OriginalSQL.Free;
  end;
end;

procedure TADSform.ExecuteBtnClick(Sender: TObject);
var
  ErrorMsg: string;
  CleanSQL: string;
  RecordCount: Integer;
begin
     if RadioGroup1.ItemIndex = 0 then
     begin
         ADOQuery.Active := False;
         ADOTable.Active := False;
         
         // Use CleanConnectionString for connection strings
         ADOQuery.ConnectionString := CleanConnectionString(ConnectStr.Text);
         
         // Use CleanSQLString for SQL commands
         CleanSQL := CleanSQLString(SqlCommand.Text);
         
         // Validate SQL before executing
         if not ValidateSQLSyntax(ADOQuery, CleanSQL, ErrorMsg) then
         begin
           ShowMessage('Cannot execute SQL.' + #13#10 + ErrorMsg);
           Label6.Caption := 'SQL Results: 0 records';
           Exit;
         end;
         
         ADOQuery.SQL.Clear;
         ADOQuery.SQL.Add(CleanSQL);
         DataSource.DataSet := ADOQuery;
         ADOQuery.Active := True;
         
         // Get record count and update label
         RecordCount := ADOQuery.RecordCount;
         Label6.Caption := 'SQL Results: ' + IntToStr(RecordCount) + ' records';
     end 
     else
     begin
         ADOQuery.Active := False;
         ADOTable.Active := False;
         
         // Use CleanConnectionString for connection strings
         ADOTable.ConnectionString := CleanConnectionString(ConnectStr.Text);
         
         ADOTable.TableName := TableName.Text;
         ADOTable.IndexName := TableIndex.Text;
         ADOTable.Filter    := TableFilter.Text;
         if length(ADOTable.Filter) > 0 then 
           ADOTable.Filtered := True 
         else 
           ADOTable.Filtered := False;
         DataSource.DataSet := ADOTable;
         ADOTable.Active := True;
         
         // Get record count for table mode
         RecordCount := ADOTable.RecordCount;
         Label6.Caption := 'SQL Results: ' + IntToStr(RecordCount) + ' records';
     end;
end;

procedure TADSform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    // Close all active datasets
    if ADOQuery.Active then
      ADOQuery.Active := False;
    
    if ADOTable.Active then
      ADOTable.Active := False;
    
    // Close the connection if it's open
    if ADOConnection1.Connected then
      ADOConnection1.Connected := False;
      
  except
    on E: Exception do
    begin
      // Silently handle any errors during cleanup
      // Or optionally log them
    end;
  end;
  
  Action := caFree;
end;

procedure TADSform.ExportCsvBtnClick(Sender: TObject);
var
  CSVFile: TextFile;
  i, j: Integer;
  LineText: string;
  FieldValue: string;
  ActiveDataSet: TDataSet;
  RecordsProcessed: Integer;
begin
  // Check if there's an active dataset with data
  if not Assigned(DataSource.DataSet) then
  begin
    ShowMessage('No data to export. Please execute a query first.');
    Exit;
  end;

  ActiveDataSet := DataSource.DataSet;
  
  if not ActiveDataSet.Active then
  begin
    ShowMessage('No data to export. Please execute a query first.');
    Exit;
  end;

  if ActiveDataSet.RecordCount = 0 then
  begin
    ShowMessage('No records to export.');
    Exit;
  end;

  // Configure the Save Dialog
  ExportSaveDialog.Title := 'Export to CSV';
  ExportSaveDialog.Filter := 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*';
  ExportSaveDialog.DefaultExt := 'csv';
  ExportSaveDialog.FileName := 'export_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.csv';
  
  // Show the save dialog
  if not ExportSaveDialog.Execute then
    Exit;

  Screen.Cursor := crHourGlass;
  try
    AssignFile(CSVFile, ExportSaveDialog.FileName);
    Rewrite(CSVFile);
    
    try
      // Write header row (field names)
      LineText := '';
      for i := 0 to ActiveDataSet.FieldCount - 1 do
      begin
        if i > 0 then
          LineText := LineText + ',';
        LineText := LineText + '"' + ActiveDataSet.Fields[i].FieldName + '"';
      end;
      Writeln(CSVFile, LineText);
      
      // Write data rows
      RecordsProcessed := 0;
      ActiveDataSet.First;
      while not ActiveDataSet.Eof do
      begin
        LineText := '';
        for j := 0 to ActiveDataSet.FieldCount - 1 do
        begin
          if j > 0 then
            LineText := LineText + ',';
          
          // Get field value and handle nulls
          if ActiveDataSet.Fields[j].IsNull then
            FieldValue := ''
          else
            FieldValue := ActiveDataSet.Fields[j].AsString;
          
          // Escape quotes and wrap in quotes if contains comma, quote, or newline
          FieldValue := StringReplace(FieldValue, '"', '""', [rfReplaceAll]);
          if (Pos(',', FieldValue) > 0) or 
             (Pos('"', FieldValue) > 0) or 
             (Pos(#13, FieldValue) > 0) or 
             (Pos(#10, FieldValue) > 0) then
            FieldValue := '"' + FieldValue + '"'
          else if Length(FieldValue) > 0 then
            FieldValue := '"' + FieldValue + '"';
          
          LineText := LineText + FieldValue;
        end;
        Writeln(CSVFile, LineText);
        
        Inc(RecordsProcessed);
        
        // Update label every 100 records for progress indication
        if RecordsProcessed mod 100 = 0 then
        begin
          Label6.Caption := 'Exporting: ' + IntToStr(RecordsProcessed) + ' records...';
          Application.ProcessMessages;
        end;
        
        ActiveDataSet.Next;
      end;
      
      // Restore original label
      Label6.Caption := 'SQL Results: ' + IntToStr(ActiveDataSet.RecordCount) + ' records';
      
      ShowMessage('Export completed successfully!' + #13#10 + 
                  'File: ' + ExportSaveDialog.FileName + #13#10 +
                  'Records exported: ' + IntToStr(RecordsProcessed));
      
    finally
      CloseFile(CSVFile);
      // Return to first record
      ActiveDataSet.First;
    end;
    
  except
    on E: Exception do
    begin
      ShowMessage('Error exporting to CSV: ' + E.Message);
      Label6.Caption := 'SQL Results: ' + IntToStr(ActiveDataSet.RecordCount) + ' records';
    end;
  end;
  
  Screen.Cursor := crDefault;
end;

end.

