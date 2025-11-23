# CLAUDE.md - ADS SQL Commander Project Context

## Project Overview

**ADS SQL Commander** (formerly "ADS Query Tool") is a desktop database utility built with **Delphi 7** for querying and managing Advantage Database Server (ADS) databases. The application provides a user-friendly interface for executing SQL queries, browsing tables, and exporting data to CSV format.

## Project Structure

```
ADS_SQL_Commander/
├── ADSSQLCommander.dpr          # Main program entry point
├── MainForm.pas                  # Core application logic (363 lines)
├── MainForm.dfm                  # Form UI definition
├── ADSSQLCommander.exe          # Compiled executable (934 KB)
├── ADSSQLCommander.res          # Resource file
├── adscmd.ico                   # Application icons
├── adssqlcmdr_icon.ico
├── adssqlcmdr_icon.svg
├── README.md                    # Comprehensive user documentation
└── .gitignore                   # Git exclusions
```

## Technology Stack

- **Language**: Object Pascal (Delphi 7)
- **Database**: Advantage Database Server (ADS)
- **Data Access**: ADO (ActiveX Data Objects) via Advantage OLEDB Provider
- **UI Framework**: VCL (Visual Component Library)
- **Platform**: Windows (XP or later)

## Core Components

### Form Components (TADSform)

**Data Components:**
- `ADOConnection1: TADOConnection` - Database connection (configured via connection string)
- `ADOQuery: TADOQuery` - SQL query execution
- `ADOTable: TADOTable` - Direct table browsing
- `DataSource: TDataSource` - Data binding layer
- `DBGridSqlResults: TDBGrid` - Results display grid

**UI Controls:**
- `ConnectString: TMemo` - Multi-line connection string input
- `PageControlSql: TPageControl` - Tabbed container for multiple SQL queries
- `AddTabBtn: TButton` - Add new SQL query tab
- `RemoveTabBtn: TButton` - Remove current SQL query tab
- `LoadSqlBtn: TButton` - Load SQL file into active tab
- `SaveSqlBtn: TButton` - Save active tab's SQL to file
- `RadioGroup1: TRadioGroup` - Mode selection (Query/Table)
- `TableName: TEdit` - Table name for browse mode
- `TableIndex: TEdit` - Index specification
- `TableFilter: TEdit` - Filter expression
- `ExecuteBtn: TButton` - Execute query/browse
- `ExportCsvBtn: TButton` - CSV export trigger
- `CloseAllBtn: TBitBtn` - Close datasets
- `ExportSaveDialog: TSaveDialog` - CSV export file dialog
- `OpenSqlDialog: TOpenDialog` - SQL file open dialog
- `SaveSqlDialog: TSaveDialog` - SQL file save dialog
- `LabelSqlResults: TLabel` - Record count and status display

## Key Features

### 1. Multiple SQL Query Tabs
- **Tabbed Interface**: `PageControlSql` with multiple `TTabSheet` pages
- **Add Tabs**: Click "+ Add Tab" button to create new SQL query tabs
- **Remove Tabs**: Click "- Remove Tab" button to remove current tab (with confirmation)
- **Tab Switching**: Click on any tab to switch between different SQL queries
- **Active Query Execution**: Execute button runs the SQL from the currently active tab
- **Dynamic Tab Management**: Create and remove tabs at runtime as needed
- **Load/Save SQL Files**:
  - Click "Load SQL..." to load a .sql file into the active tab
  - Click "Save SQL..." to save the active tab's content to a .sql file
  - Supports .sql, .txt, and all file types
  - Automatic filename suggestion based on tab caption (e.g., "Query_1.sql")
- Implementation:
  - `GetActiveSqlMemo()` - Helper function to retrieve active tab's TMemo ([MainForm.pas:162-184](MainForm.pas#L162-L184))
  - `AddTabBtnClick()` - Creates new tab with empty TMemo ([MainForm.pas:192-228](MainForm.pas#L192-L228))
  - `RemoveTabBtnClick()` - Removes active tab with confirmation ([MainForm.pas:230-258](MainForm.pas#L230-L258))
  - `LoadSqlBtnClick()` - Loads SQL file into active tab ([MainForm.pas:260-298](MainForm.pas#L260-L298))
  - `SaveSqlBtnClick()` - Saves active tab to SQL file ([MainForm.pas:300-348](MainForm.pas#L300-L348))

### 2. Dual Query Modes
- **Query Mode** (RadioGroup1.ItemIndex = 0): Execute custom SQL commands
- **Table Mode** (RadioGroup1.ItemIndex = 1): Direct table access with filtering

### 3. SQL Validation System

Two-tier validation approach:

**Client-Side** (`ValidateSQLBasic`):
- Checks for SQL keywords (SELECT, INSERT, UPDATE, DELETE, EXECUTE)
- Validates bracket matching: `(` vs `)`
- Validates quote matching (must be even number)

**Server-Side** (`ValidateSQLSyntax`):
- Uses `ADOQuery.Prepared := True` to catch syntax errors
- Returns detailed error messages from database
- Non-destructive testing (restores original SQL state)

### 4. String Processing Functions

**`CleanConnectionString(const MemoText: string): string`**
- Removes ALL line breaks (#13, #10, #13#10)
- Essential for ADO connection string parsing
- Location: [MainForm.pas:48-55](MainForm.pas#L48-L55)

**`CleanSQLString(const MemoText: string): string`**
- Converts multi-line SQL to single-line format
- Replaces line breaks with spaces
- Collapses multiple consecutive spaces
- Preserves SQL syntax integrity
- Location: [MainForm.pas:57-71](MainForm.pas#L57-L71)

### 5. CSV Export (`ExportCsvBtnClick`)

**Features:**
- RFC 4180 compliant CSV formatting
- Proper handling of nulls, commas, quotes, line breaks
- Progress indication (updates every 100 records)
- Timestamped default filenames: `export_yyyymmdd_hhnnss.csv`
- Location: [MainForm.pas:240-361](MainForm.pas#L240-L361)

**Export Algorithm:**
1. Validate active dataset exists with data
2. Write header row with field names (quoted)
3. Iterate through records, escaping special characters
4. Show progress every 100 records
5. Display completion summary

### 6. Graceful Shutdown (`FormClose`)

- Closes `ADOQuery` and `ADOTable` if active
- Disconnects `ADOConnection1` if connected
- Silently handles cleanup errors
- Location: [MainForm.pas:215-238](MainForm.pas#L215-L238)

## Current State

### Working Features
✅ SQL query execution with validation
✅ Table browsing with index/filter support
✅ CSV export functionality
✅ Connection string management
✅ Record count display
✅ Copy/paste from grid (Ctrl+C/Ctrl+V)
✅ Error handling and messaging

### Modified Files (Git Status)
- `MainForm.dfm` - Modified (likely UI changes)

### Recent Commits
- **94c51c0**: "Initial commit: ADS SQL Commander v1.0"

## Code Quality Notes

### Strengths
- Clean separation of UI and data logic
- Comprehensive error handling with try-except blocks
- Resource cleanup in FormClose
- Helper functions for string processing
- SQL validation before execution prevents runtime errors

### Areas for Consideration
- No query history or favorites functionality
- Connection string stored in plain text memo (no encryption)
- Single database connection at a time
- No SQL syntax highlighting in memo fields
- No execution time tracking

## Usage Patterns

### Typical Workflow

1. **Connect**: Enter connection string in `ConnectStr` memo
   ```
   Provider=Advantage.OLEDB.1;
   User ID=admin;
   Password=admin;
   Data Source=C:\Path\To\Database.add;
   ```

2. **Query**: Select Query mode, enter SQL in the active query tab
   ```sql
   SELECT item, description
   FROM arinvt01
   WHERE item = '0900-604'
   ```

3. **Execute**: Click Execute button, view results in DBGridSqlResults

4. **Manage Multiple Queries**:
   - Click "+ Add Tab" to create a new SQL query tab
   - Click on any tab to switch between queries
   - Click "- Remove Tab" to delete the current tab (requires confirmation)
   - Each tab maintains its own SQL content independently

5. **Load/Save SQL Files**:
   - Click "Load SQL..." to open a .sql file into the active tab
   - Click "Save SQL..." to save the active tab's SQL to a file
   - File dialogs support .sql, .txt, and all file types

6. **Export**: Click Export CSV button, choose location

## Future Enhancement Ideas (from README)

- Query history and favorites
- SQL syntax highlighting
- Multiple database connections
- Query result formatting options
- Direct printing of query results
- Import functionality (CSV to database)
- Query execution time tracking
- Export to additional formats (Excel, JSON, XML)

## Development Environment

- **IDE**: Delphi 7
- **VCS**: Git (current branch: main)
- **Compiled Binary**: 934 KB executable
- **Author**: Josue - MySQL Developer and Architect

## Important Implementation Details

### Connection String Handling
- Multi-line input allowed for readability
- `CleanConnectionString` removes ALL linebreaks before passing to ADO
- Critical for proper ADO parsing

### SQL Command Processing
- Multi-line SQL supported in memo field
- `CleanSQLString` preserves spacing between keywords
- Validation occurs before execution to prevent errors

### Record Count Display
- Updated after each Execute operation
- Format: "SQL Results: {count} records"
- Shows export progress during CSV generation

## Notes for Claude

- This is a single-form Delphi 7 application
- All core logic resides in [MainForm.pas](MainForm.pas)
- Uses ADO components for database connectivity
- Connection string format follows Advantage OLEDB Provider syntax
- Error messages displayed via `ShowMessage()` dialogs
- Grid supports standard Windows clipboard operations

---

**Last Updated**: 2025-11-22
**Version**: 1.0
**Status**: Production-ready with pending UI modifications
