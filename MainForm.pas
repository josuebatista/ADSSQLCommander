unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, DBTables, ADODB, ExtCtrls, Buttons,
  ComCtrls;

type
  TADSform = class(TForm)
    DataSource: TDataSource;
    DBGridSqlResults: TDBGrid;
    ExecuteBtn: TButton;
    ADOQuery: TADOQuery;
    ADOConnection1: TADOConnection;
    ADOTable: TADOTable;
    RadioGroup1: TRadioGroup;
    TableName: TComboBox;
    Label1: TLabel;
    LabelSqlCommand: TLabel;
    TableIndex: TEdit;
    Label3: TLabel;
    TableFilter: TEdit;
    Label4: TLabel;
    ConnectString: TMemo;
    LabelConnectionStrring: TLabel;
    LabelSqlResults: TLabel;
    CloseAllBtn: TBitBtn;
    ExportCsvBtn: TButton;
    ExportSaveDialog: TSaveDialog;
    PageControlSql: TPageControl;
    TabSheet1: TTabSheet;
    SqlCommand1: TMemo;
    AddTabBtn: TButton;
    RemoveTabBtn: TButton;
    LoadSqlBtn: TButton;
    SaveSqlBtn: TButton;
    OpenSqlDialog: TOpenDialog;
    SaveSqlDialog: TSaveDialog;
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExportCsvBtnClick(Sender: TObject);
    procedure AddTabBtnClick(Sender: TObject);
    procedure RemoveTabBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadSqlBtnClick(Sender: TObject);
    procedure SaveSqlBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
    function GetActiveSqlMemo: TMemo;
    procedure PopulateTableList;
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

function TADSform.GetActiveSqlMemo: TMemo;
var
  ActiveTab: TTabSheet;
  i: Integer;
begin
  Result := nil;
  if PageControlSql.PageCount = 0 then
    Exit;

  ActiveTab := PageControlSql.ActivePage;
  if not Assigned(ActiveTab) then
    Exit;

  // Find the memo in the active tab
  for i := 0 to ActiveTab.ControlCount - 1 do
  begin
    if ActiveTab.Controls[i] is TMemo then
    begin
      Result := TMemo(ActiveTab.Controls[i]);
      Exit;
    end;
  end;
end;

procedure TADSform.FormCreate(Sender: TObject);
begin
  // Initial tab is already created in the DFM
  // Nothing special needed here for now
end;

procedure TADSform.AddTabBtnClick(Sender: TObject);
var
  NewTab: TTabSheet;
  NewMemo: TMemo;
  TabCount: Integer;
begin
  TabCount := PageControlSql.PageCount + 1;

  // Create new tab sheet
  NewTab := TTabSheet.Create(PageControlSql);
  NewTab.PageControl := PageControlSql;
  NewTab.Caption := 'Query ' + IntToStr(TabCount);

  // Create memo for the new tab
  NewMemo := TMemo.Create(NewTab);
  NewMemo.Parent := NewTab;
  NewMemo.Align := alClient;
  NewMemo.Font.Charset := ANSI_CHARSET;
  NewMemo.Font.Name := 'Courier';
  NewMemo.Font.Height := -12;
  NewMemo.ScrollBars := ssBoth;
  NewMemo.WordWrap := False;
  NewMemo.Lines.Clear;

  // Make the new tab active
  PageControlSql.ActivePage := NewTab;
end;

procedure TADSform.RemoveTabBtnClick(Sender: TObject);
var
  ActiveTab: TTabSheet;
  TabIndex: Integer;
begin
  // Don't allow removing the last tab
  if PageControlSql.PageCount <= 1 then
  begin
    ShowMessage('Cannot remove the last tab.');
    Exit;
  end;

  ActiveTab := PageControlSql.ActivePage;
  if not Assigned(ActiveTab) then
    Exit;

  if MessageDlg('Remove tab "' + ActiveTab.Caption + '"?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    TabIndex := PageControlSql.ActivePageIndex;
    ActiveTab.Free;

    // Select the previous tab, or first tab if we removed the first one
    if TabIndex > 0 then
      PageControlSql.ActivePageIndex := TabIndex - 1
    else if PageControlSql.PageCount > 0 then
      PageControlSql.ActivePageIndex := 0;
  end;
end;

procedure TADSform.LoadSqlBtnClick(Sender: TObject);
var
  ActiveMemo: TMemo;
  SqlFile: TextFile;
  Line: string;
  FileContent: TStringList;
begin
  // Get the active SQL memo
  ActiveMemo := GetActiveSqlMemo;
  if not Assigned(ActiveMemo) then
  begin
    ShowMessage('No SQL tab available.');
    Exit;
  end;

  // Show the open dialog
  if not OpenSqlDialog.Execute then
    Exit;

  try
    FileContent := TStringList.Create;
    try
      // Load the file into the string list
      FileContent.LoadFromFile(OpenSqlDialog.FileName);

      // Load the content into the active memo
      ActiveMemo.Lines.Assign(FileContent);

      ShowMessage('SQL file loaded successfully!' + #13#10 +
                  'File: ' + OpenSqlDialog.FileName + #13#10 +
                  'Lines: ' + IntToStr(FileContent.Count));
    finally
      FileContent.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Error loading SQL file: ' + E.Message);
  end;
end;

procedure TADSform.SaveSqlBtnClick(Sender: TObject);
var
  ActiveMemo: TMemo;
  FileContent: TStringList;
begin
  // Get the active SQL memo
  ActiveMemo := GetActiveSqlMemo;
  if not Assigned(ActiveMemo) then
  begin
    ShowMessage('No SQL tab available.');
    Exit;
  end;

  // Check if there's content to save
  if ActiveMemo.Lines.Count = 0 then
  begin
    if MessageDlg('The SQL tab is empty. Save anyway?',
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;

  // Configure the Save Dialog with a default filename based on tab caption
  SaveSqlDialog.FileName := StringReplace(
    PageControlSql.ActivePage.Caption, ' ', '_', [rfReplaceAll]) + '.sql';

  // Show the save dialog
  if not SaveSqlDialog.Execute then
    Exit;

  try
    FileContent := TStringList.Create;
    try
      // Copy memo content to string list
      FileContent.Assign(ActiveMemo.Lines);

      // Save to file
      FileContent.SaveToFile(SaveSqlDialog.FileName);

      ShowMessage('SQL file saved successfully!' + #13#10 +
                  'File: ' + SaveSqlDialog.FileName + #13#10 +
                  'Lines: ' + IntToStr(FileContent.Count));
    finally
      FileContent.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Error saving SQL file: ' + E.Message);
  end;
end;

procedure TADSform.PopulateTableList;
var
  SearchRec: TSearchRec;
  DataPath: string;
  ConnStr: string;
  PosStart, PosEnd: Integer;
  TableFileName: string;
begin
  TableName.Items.Clear;
  TableName.Text := '';

  // Check if connection string is provided
  if Trim(ConnectString.Text) = '' then
  begin
    ShowMessage('Please enter a connection string first.');
    Exit;
  end;

  try
    // Extract the Data Source path from the connection string
    ConnStr := UpperCase(CleanConnectionString(ConnectString.Text));
    PosStart := Pos('DATA SOURCE=', ConnStr);

    if PosStart = 0 then
    begin
      ShowMessage('Cannot find Data Source in connection string.');
      Exit;
    end;

    PosStart := PosStart + Length('DATA SOURCE=');
    PosEnd := Pos(';', Copy(ConnStr, PosStart, Length(ConnStr)));

    if PosEnd > 0 then
      DataPath := Copy(CleanConnectionString(ConnectString.Text), PosStart, PosEnd - 1)
    else
      DataPath := Copy(CleanConnectionString(ConnectString.Text), PosStart, Length(ConnStr) - PosStart + 1);

    DataPath := Trim(DataPath);

    // Extract directory path from the .add file
    DataPath := ExtractFilePath(DataPath);

    if not DirectoryExists(DataPath) then
    begin
      ShowMessage('Database directory not found: ' + DataPath);
      Exit;
    end;

    // Search for table files (.adt for ADT format, .dbf for DBF format)
    // First look for .adt files (Advantage native format)
    if FindFirst(DataPath + '*.adt', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          TableFileName := ChangeFileExt(SearchRec.Name, '');
          TableName.Items.Add(TableFileName);
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    // Also look for .dbf files (DBF format)
    if FindFirst(DataPath + '*.dbf', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          TableFileName := ChangeFileExt(SearchRec.Name, '');
          // Only add if not already in the list (in case both .adt and .dbf exist)
          if TableName.Items.IndexOf(TableFileName) = -1 then
            TableName.Items.Add(TableFileName);
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    // Sort alphabetically
    TableName.Sorted := True;

    if TableName.Items.Count > 0 then
      ShowMessage('Found ' + IntToStr(TableName.Items.Count) + ' tables in: ' + DataPath)
    else
      ShowMessage('No table files found in: ' + DataPath + #13#10 +
                  'Looking for .adt and .dbf files.');

  except
    on E: Exception do
    begin
      ShowMessage('Error retrieving table list: ' + E.Message);
    end;
  end;
end;

procedure TADSform.RadioGroup1Click(Sender: TObject);
begin
  // When ADO Table mode is selected (ItemIndex = 1), populate the table list
  if RadioGroup1.ItemIndex = 1 then
  begin
    PopulateTableList;
  end;
end;

procedure ClearDataSetFields(DataSet: TDataSet);
var
  i: Integer;
  TempField: TField;
begin
  // Close the dataset first
  if DataSet.Active then
    DataSet.Close;

  // Destroy all field components in reverse order
  for i := DataSet.FieldCount - 1 downto 0 do
  begin
    TempField := DataSet.Fields[i];
    if Assigned(TempField) then
    begin
      TempField.DataSet := nil;  // Disconnect from dataset first
      TempField.Free;
    end;
  end;

  // Clear field definitions as well
  if DataSet is TADODataSet then
    TADODataSet(DataSet).FieldDefs.Clear;
end;

procedure TADSform.ExecuteBtnClick(Sender: TObject);
var
  ErrorMsg: string;
  CleanSQL: string;
  RecordCount: Integer;
  ActiveMemo: TMemo;
begin
     if RadioGroup1.ItemIndex = 0 then
     begin
         // Clear both datasets to prevent conflicts
         ClearDataSetFields(ADOTable);
         ClearDataSetFields(ADOQuery);

         // Get the active SQL memo from the current tab
         ActiveMemo := GetActiveSqlMemo;
         if not Assigned(ActiveMemo) then
         begin
           ShowMessage('No SQL tab available.');
           Exit;
         end;

         // Use CleanConnectionString for connection strings
         ADOQuery.ConnectionString := CleanConnectionString(ConnectString.Text);

         // Use CleanSQLString for SQL commands from active tab
         CleanSQL := CleanSQLString(ActiveMemo.Text);

         // Validate SQL before executing
         if not ValidateSQLSyntax(ADOQuery, CleanSQL, ErrorMsg) then
         begin
           ShowMessage('Cannot execute SQL.' + #13#10 + ErrorMsg);
           LabelSqlResults.Caption := 'SQL Results: 0 records';
           Exit;
         end;

         ADOQuery.SQL.Clear;
         ADOQuery.SQL.Add(CleanSQL);
         DataSource.DataSet := ADOQuery;
         ADOQuery.Active := True;

         // Get record count and update label
         RecordCount := ADOQuery.RecordCount;
         LabelSqlResults.Caption := 'SQL Results: ' + IntToStr(RecordCount) + ' records';
     end
     else
     begin
         try
           // Clear both datasets to prevent conflicts
           ClearDataSetFields(ADOQuery);
           ClearDataSetFields(ADOTable);

           // Clear previous table settings
           ADOTable.TableName := '';
           ADOTable.IndexName := '';
           ADOTable.Filter := '';
           ADOTable.Filtered := False;

           // Force recreation by changing connection
           ADOTable.ConnectionString := '';
           Application.ProcessMessages;  // Allow cleanup to complete

           // Use CleanConnectionString for connection strings
           ADOTable.ConnectionString := CleanConnectionString(ConnectString.Text);

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
           LabelSqlResults.Caption := 'SQL Results: ' + IntToStr(RecordCount) + ' records';

         except
           on E: Exception do
           begin
             // If we still get a component naming error, try one more time with full reset
             if Pos('already exists', E.Message) > 0 then
             begin
               ShowMessage('Component conflict detected. Attempting to recover...');

               // Nuclear option: clear everything again
               ClearDataSetFields(ADOTable);
               ADOTable.Close;
               ADOTable.ConnectionString := '';
               Application.ProcessMessages;

               // Try again
               ADOTable.ConnectionString := CleanConnectionString(ConnectString.Text);
               ADOTable.TableName := TableName.Text;
               ADOTable.IndexName := TableIndex.Text;
               ADOTable.Filter := TableFilter.Text;
               if length(ADOTable.Filter) > 0 then
                 ADOTable.Filtered := True
               else
                 ADOTable.Filtered := False;
               DataSource.DataSet := ADOTable;

               try
                 ADOTable.Active := True;
                 RecordCount := ADOTable.RecordCount;
                 LabelSqlResults.Caption := 'SQL Results: ' + IntToStr(RecordCount) + ' records';
               except
                 on E2: Exception do
                 begin
                   ShowMessage('Error opening table: ' + E2.Message);
                   LabelSqlResults.Caption := 'SQL Results: Error';
                 end;
               end;
             end
             else
               raise;  // Re-raise if it's a different error
           end;
         end;
     end;
end;

procedure TADSform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    // Clear all field components before closing
    ClearDataSetFields(ADOQuery);
    ClearDataSetFields(ADOTable);

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
          LabelSqlResults.Caption := 'Exporting: ' + IntToStr(RecordsProcessed) + ' records...';
          Application.ProcessMessages;
        end;
        
        ActiveDataSet.Next;
      end;
      
      // Restore original label
      LabelSqlResults.Caption := 'SQL Results: ' + IntToStr(ActiveDataSet.RecordCount) + ' records';
      
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
      LabelSqlResults.Caption := 'SQL Results: ' + IntToStr(ActiveDataSet.RecordCount) + ' records';
    end;
  end;
  
  Screen.Cursor := crDefault;
end;

end.

