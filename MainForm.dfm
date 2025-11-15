object ADSform: TADSform
  Left = 1437
  Top = 63
  Width = 1200
  Height = 800
  Caption = 'ADS SQL Commander'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 400
    Top = 24
    Width = 27
    Height = 13
    Caption = 'Table'
  end
  object Label2: TLabel
    Left = 16
    Top = 166
    Width = 83
    Height = 13
    Caption = 'SQL Command'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 600
    Top = 24
    Width = 56
    Height = 13
    Caption = 'Table Index'
  end
  object Label4: TLabel
    Left = 376
    Top = 56
    Width = 52
    Height = 13
    Caption = 'Table Filter'
  end
  object Label5: TLabel
    Left = 16
    Top = 104
    Width = 102
    Height = 13
    Caption = 'Connection String'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 16
    Top = 327
    Width = 71
    Height = 13
    Caption = 'SQL Results'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object DBGrid1: TDBGrid
    Left = 17
    Top = 343
    Width = 1150
    Height = 404
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object ExecuteBtn: TButton
    Left = 792
    Top = 11
    Width = 100
    Height = 25
    Caption = 'Execute v.015a'
    TabOrder = 1
    OnClick = ExecuteBtnClick
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 16
    Width = 97
    Height = 57
    Caption = 'RadioGroup1'
    ItemIndex = 0
    Items.Strings = (
      'ADO Query'
      'ADO Table')
    TabOrder = 2
  end
  object TableName: TEdit
    Left = 432
    Top = 16
    Width = 145
    Height = 21
    TabOrder = 3
  end
  object TableIndex: TEdit
    Left = 664
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object TableFilter: TEdit
    Left = 432
    Top = 48
    Width = 441
    Height = 21
    TabOrder = 5
  end
  object ConnectStr: TMemo
    Left = 17
    Top = 120
    Width = 1150
    Height = 41
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'Courier'
    Font.Style = []
    Lines.Strings = (
      
        'Provider=Advantage.OLEDB.1;Password=admin;User ID=admin;Data Sou' +
        'rce=C:\Workarea\ADS_Delphi\AdvantageData\01.add;Advantage Server' +
        ' '
      'Type=ADS_LOCAL_SERVER;')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object SqlCommand: TMemo
    Left = 16
    Top = 182
    Width = 1153
    Height = 137
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier'
    Font.Style = []
    Lines.Strings = (
      'select * from arinvt01')
    ParentFont = False
    TabOrder = 7
  end
  object CloseAllBtn: TBitBtn
    Left = 1067
    Top = 80
    Width = 100
    Height = 25
    Caption = '&Close All'
    TabOrder = 8
    Kind = bkClose
  end
  object ExportCsvBtn: TButton
    Left = 904
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Export to CSV'
    TabOrder = 9
    OnClick = ExportCsvBtnClick
  end
  object DataSource: TDataSource
    DataSet = ADOTable
    Left = 296
    Top = 480
  end
  object ADOQuery: TADOQuery
    ConnectionString = 
      'Provider=Advantage.OLEDB.1;User ID=admin;Password=admin;Data Sou' +
      'rce=C:\Workarea\ADS_Delphi\AdvantageData\01.add;Persist Security' +
      ' Info=False;Advantage Server Type=ADS_LOCAL_SERVER;'
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'select custno from arcusto')
    Left = 264
    Top = 480
  end
  object ADOConnection1: TADOConnection
    ConnectionString = 
      'Provider=Advantage.OLEDB.1;User ID=admin;Data Source=C:\Workarea' +
      '\ADS_Delphi\AdvantageData\01.add;Persist Security Info=False;Adv' +
      'antage Server Type=ADS_LOCAL_SERVER;'
    DefaultDatabase = '01.add'
    LoginPrompt = False
    Mode = cmReadWrite
    Provider = 'Advantage.OLEDB.1'
    Left = 328
    Top = 480
  end
  object ADOTable: TADOTable
    AutoCalcFields = False
    ConnectionString = 
      'Provider=Advantage.OLEDB.1;User ID=admin;Password=admin;Data Sou' +
      'rce=C:\Workarea\ADS_Delphi\AdvantageData\01.add;Persist Security' +
      ' Info=False;Advantage Server Type=ADS_LOCAL_SERVER;'
    CursorType = ctStatic
    LockType = ltReadOnly
    TableName = 'arinvt01'
    Left = 368
    Top = 480
  end
  object ExportSaveDialog: TSaveDialog
    Left = 408
    Top = 480
  end
end
