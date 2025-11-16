# ADS Query Tool

A powerful Delphi 7 database utility for querying and managing Advantage Database Server (ADS) databases with advanced SQL execution and data export capabilities.

## Overview

ADS Query Tool is a desktop application built with Delphi 7 that provides a user-friendly interface for executing SQL queries and browsing tables in Advantage Database Server databases. The tool features robust error handling, SQL syntax validation, and CSV export functionality.

## Features

### Core Functionality

- **Dual Query Modes**
  - **SQL Query Mode**: Execute custom SQL commands with full syntax validation
  - **Table Browse Mode**: Direct table access with filtering and indexing capabilities

- **Database Connection Management**
  - Support for ADO connections via Advantage OLEDB Provider
  - Connection string configuration through multi-line memo field
  - Automatic connection cleanup on application close

### SQL Query Features

- **Advanced SQL Editor**
  - Multi-line SQL command input with proper formatting preservation
  - Automatic line break handling that maintains SQL syntax integrity
  - Support for complex queries with proper spacing and readability

- **SQL Syntax Validation**
  - Client-side validation for basic SQL structure
  - Server-side validation using prepared statements
  - Bracket and quote matching verification
  - Detailed error messaging for syntax issues

- **Query Results**
  - Real-time record count display
  - Interactive data grid with sortable columns
  - Copy cell values directly from grid (Ctrl+C / Ctrl+V)
  - Seamless navigation through query results

### Table Browse Mode

- **Direct Table Access**
  - Table name selection
  - Index specification for optimized browsing
  - Custom filter expressions
  - Automatic filter activation/deactivation

### Data Export

- **CSV Export Functionality**
  - One-click export to CSV format
  - Automatic field name headers
  - RFC 4180 compliant CSV formatting
  - Proper handling of:
    - Null values (exported as empty strings)
    - Commas in data (field quoting)
    - Quotes in data (escape characters)
    - Line breaks in data (preserved with quoting)
  - Timestamped default filenames (`export_yyyymmdd_hhnnss.csv`)
  - Progress indication for large datasets
  - Compatible with Excel, Google Sheets, and other spreadsheet applications

### User Interface

- **Intuitive Layout**
  - Clean, organized interface with labeled controls
  - Radio group for mode selection (Query/Table)
  - Results grid with automatic column sizing
  - Status label showing record counts
  - Standard Windows dialogs for file operations

- **Data Grid Features**
  - Built-in copy/paste support (Ctrl+C / Ctrl+V)
  - Cell selection and navigation
  - Column resizing and sorting
  - Scroll support for large datasets

## Technical Specifications

### Requirements

- **Operating System**: Windows (XP or later)
- **Development Environment**: Delphi 7
- **Database**: Advantage Database Server (ADS)
- **Provider**: Advantage OLEDB Provider 1.x or higher

### Components Used

- `TADOConnection` - Database connection management
- `TADOQuery` - SQL query execution
- `TADOTable` - Direct table access
- `TDataSource` - Data binding
- `TDBGrid` - Results display
- `TMemo` - Multi-line text input (Connection string, SQL commands)
- `TEdit` - Single-line input (Table settings)
- `TRadioGroup` - Mode selection
- `TSaveDialog` - File save operations
- `TButton` / `TBitBtn` - Action triggers

### Key Functions

#### Connection String Management
```delphi
CleanConnectionString(const MemoText: string): string
```
Removes all line breaks from connection strings to ensure proper ADO parsing.

#### SQL String Formatting
```delphi
CleanSQLString(const MemoText: string): string
```
Converts multi-line SQL to single-line format while preserving proper spacing between keywords.

#### SQL Validation
```delphi
ValidateSQLBasic(const SQLText: string): Boolean
ValidateSQLSyntax(AQuery: TADOQuery; const SQLText: string; out ErrorMsg: string): Boolean
```
Two-tier validation system providing both client-side and server-side SQL syntax checking.

## Usage

### Connecting to a Database

1. Enter your Advantage OLEDB connection string in the **ConnectStr** memo field:
   ```
   Provider=Advantage.OLEDB.1;
   User ID=admin;
   Password=admin;
   Data Source=C:\Path\To\Database.add;
   Persist Security Info=False;
   Advantage Server Type=ADS_LOCAL_SERVER;
   ```

### Executing SQL Queries

1. Select the **Query** radio button
2. Enter your SQL command in the **SqlCommand** memo field:
   ```sql
   SELECT item, description
   FROM arinvt01
   WHERE item = '0900-604'
   ```
3. Click **Execute** button
4. Results appear in the data grid with record count displayed

### Browsing Tables

1. Select the **Table** radio button
2. Enter the table name in the **TableName** field
3. (Optional) Specify an index in the **TableIndex** field
4. (Optional) Enter a filter expression in the **TableFilter** field
5. Click **Execute** button

### Exporting Data

1. Execute a query or open a table
2. Click the **Export CSV** button
3. Choose a location and filename in the save dialog
4. Click **Save**
5. Confirmation message displays with export statistics

### Copying Cell Values

- **Method 1**: Select a cell in the grid, press Ctrl+C, click in SqlCommand memo, press Ctrl+V
- **Method 2**: Select a cell and use standard Windows clipboard operations

## Error Handling

The application includes comprehensive error handling for:

- Invalid connection strings
- SQL syntax errors (with detailed error messages)
- Empty or malformed queries
- Database connection failures
- File I/O errors during export
- Missing or inaccessible tables

## Best Practices

1. **Connection Strings**: Use multi-line format for readability; the application automatically formats them correctly
2. **SQL Queries**: Write queries in readable multi-line format; spacing is preserved automatically
3. **Large Datasets**: The application handles large result sets efficiently with progress indication during export
4. **Data Safety**: Always verify your SQL before execution; the validation system catches most errors before execution

## Graceful Shutdown

The application automatically:
- Closes all active datasets
- Disconnects database connections
- Releases all resources

No manual cleanup required when closing the application.

## Future Enhancement Ideas

- Query history and favorites
- SQL syntax highlighting
- Multiple database connections
- Query result formatting options
- Direct printing of query results
- Import functionality (CSV to database)
- Query execution time tracking
- Export to additional formats (Excel, JSON, XML)

## License

[MIT License](https://github.com/josuebatista/ADSSQLCommander/blob/main/LICENSE)

## Author

Josue - MySQL Developer and Architect

## Version History

**Version 1.0** (Current)
- Initial release
- SQL query execution with validation
- Table browse mode
- CSV export functionality
- Multi-line SQL support
- Connection string management
- Graceful connection cleanup

---

*Built with Delphi 7 for Advantage Database Server*
