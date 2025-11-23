# CLAUDE.md - ADS SQL Commander Project Context

## Project Overview

**ADS SQL Commander** (formerly "ADS Query Tool") is a desktop database utility built with **Delphi 7** for querying and managing Advantage Database Server (ADS) databases. The application provides a user-friendly interface for executing SQL queries, browsing tables, and exporting data to CSV format.

## Project Structure

```
ADS_SQL_Commander/
├── ADSSQLCommander.dpr          # Main program entry point
├── MainForm.pas                  # Core application logic (566 lines)
├── MainForm.dfm                  # Form UI definition
├── ADSSQLCommander.exe          # Compiled executable
├── ADSSQLCommander.res          # Resource file
├── adscmd.ico                   # Application icons
├── adssqlcmdr_icon.ico
├── adssqlcmdr_icon.svg
├── README.md                    # Comprehensive user documentation
├── CLAUDE.md                    # Project context for AI assistants
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
- `RadioGroup1: TRadioGroup` - Mode selection (Query/Table) with `OnClick` event
- `TableName: TComboBox` - **v1.2+** Searchable dropdown for table selection (was TEdit in v1.0-1.1)
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
  - **v1.2+**: Automatic table discovery via `PopulateTableList()` when Table mode is selected
  - **v1.2+**: Searchable/filterable dropdown with alphabetically sorted table names

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

### 6. Table List Population (`PopulateTableList`) - v1.2+

**Features:**
- File system-based table discovery for ADS Local Server
- Scans database directory for .adt (Advantage native) and .dbf (DBF format) files
- Automatic alphabetical sorting
- Duplicate prevention (if both .adt and .dbf exist for same table)
- Location: [MainForm.pas:350-442](MainForm.pas#L350-L442)

**Algorithm:**
1. Parse connection string to extract Data Source path
2. Extract directory path from .add database file
3. Scan for .adt files, add to list
4. Scan for .dbf files, add if not already present
5. Sort alphabetically and populate ComboBox
6. Display count of tables found

### 7. ADO Field Component Management (`ClearDataSetFields`) - v1.2+

**Purpose:** Prevents "component name already exists" errors when switching tables

**Features:**
- Closes dataset if active
- Disconnects fields from dataset before destroying: `TempField.DataSet := nil`
- Destroys all field components in reverse order
- Clears FieldDefs for ADO datasets
- Location: [MainForm.pas:453-476](MainForm.pas#L453-L476)

**Critical Implementation Details:**
- Called before opening any dataset (Query or Table mode)
- Reverse iteration (downto 0) prevents index shifting issues
- Used in `ExecuteBtnClick` for both modes
- Used in `FormClose` for proper cleanup
- Includes error recovery mechanism in `ExecuteBtnClick` with retry logic

### 8. Graceful Shutdown (`FormClose`)

- **v1.2+**: Clears field components via `ClearDataSetFields()`
- Closes `ADOQuery` and `ADOTable` if active
- Disconnects `ADOConnection1` if connected
- Silently handles cleanup errors
- Location: [MainForm.pas:600-620](MainForm.pas#L600-L620)

## Current State

### Working Features
✅ SQL query execution with validation
✅ Multiple SQL query tabs with load/save
✅ **v1.2+** Searchable table list with automatic discovery
✅ Table browsing with index/filter support
✅ CSV export functionality
✅ Connection string management
✅ Record count display
✅ Copy/paste from grid (Ctrl+C/Ctrl+V)
✅ **v1.2+** Robust field component management
✅ Error handling and messaging with recovery

### Recent Commits
- **13a566d**: "Add searchable table list and fix ADO field component conflicts (v1.2)" - 2025-11-23
- **e5d2577**: "Add multiple SQL query tabs and file management features (v1.1)" - 2025-11-22
- **94c51c0**: "Initial commit: ADS SQL Commander v1.0" - 2025-11-22

## Code Quality Notes

### Strengths
- Clean separation of UI and data logic
- Comprehensive error handling with try-except blocks
- **v1.2+**: Robust field component cleanup prevents ADO conflicts
- **v1.2+**: Automatic error recovery with retry logic
- Resource cleanup in FormClose
- Helper functions for string processing
- SQL validation before execution prevents runtime errors
- **v1.2+**: File system-based table discovery works without system schema

### Known Issues (Resolved in v1.2)
- ~~ADO field component naming conflicts when switching tables~~ ✅ **FIXED in v1.2**
  - Root cause: Dynamically created field components weren't properly destroyed
  - Solution: `ClearDataSetFields()` procedure with disconnect, destroy, and FieldDefs.Clear
  - Additional: Connection string reset + Application.ProcessMessages for cleanup cycle
  - Recovery: Automatic retry mechanism for persistent conflicts

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

4. **Browse Tables** (v1.2+):
   - Select Table mode from RadioGroup1
   - Application automatically scans database directory and populates table dropdown
   - Select table from searchable dropdown (or type to filter)
   - Optional: specify index and filter
   - Click Execute

5. **Manage Multiple Queries**:
   - Click "+ Add Tab" to create a new SQL query tab
   - Click on any tab to switch between queries
   - Click "- Remove Tab" to delete the current tab (requires confirmation)
   - Each tab maintains its own SQL content independently

6. **Load/Save SQL Files**:
   - Click "Load SQL..." to open a .sql file into the active tab
   - Click "Save SQL..." to save the active tab's SQL to a file
   - File dialogs support .sql, .txt, and all file types

7. **Export**: Click Export CSV button, choose location

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

### Important v1.2 Implementation Notes

**ADO Field Component Management:**
- Always call `ClearDataSetFields()` before opening any dataset
- This is CRITICAL to prevent "component name already exists" errors
- The issue was intermittent because it depended on field component lifecycle timing
- Solution requires: disconnect fields, destroy in reverse order, clear FieldDefs

**Table Discovery:**
- Works with ADS Local Server by scanning file system (no system schema access needed)
- Looks for both .adt (native) and .dbf (compatibility) formats
- Case-insensitive connection string parsing for Data Source extraction
- Uses `FindFirst`/`FindNext` for file system enumeration

**Error Recovery:**
- `ExecuteBtnClick` has built-in retry logic for component naming conflicts
- Detects "already exists" in error message and attempts full reset
- Uses `Application.ProcessMessages` to allow Windows message queue processing
- Connection string reset forces complete ADO internal cleanup

---

**Last Updated**: 2025-11-23
**Version**: 1.2
**Status**: Production-ready - Searchable table list with robust field management
