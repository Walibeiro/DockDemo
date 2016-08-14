object MainForm: TMainForm
  Left = 530
  Top = 233
  Caption = 'Docking Demo'
  ClientHeight = 440
  ClientWidth = 667
  Color = clBtnFace
  DockSite = True
  ParentFont = True
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDockOver = FormDockOver
  OnGetSiteInfo = FormGetSiteInfo
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterLeft: TSplitter
    Left = 141
    Top = 0
    Height = 373
    Visible = False
  end
  object SplitterBottom: TSplitter
    Left = 0
    Top = 373
    Width = 667
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    Visible = False
  end
  object SplitterRight: TSplitter
    Left = 523
    Top = 0
    Height = 373
    Align = alRight
    Visible = False
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 141
    Height = 373
    Align = alLeft
    BevelOuter = bvNone
    DockSite = True
    ParentBackground = False
    TabOrder = 0
    Visible = False
    OnGetSiteInfo = FormGetSiteInfo
    OnUnDock = PanelUnDock
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 377
    Width = 667
    Height = 63
    Align = alBottom
    BevelOuter = bvNone
    DockSite = True
    ParentBackground = False
    TabOrder = 1
    Visible = False
    OnGetSiteInfo = FormGetSiteInfo
    OnUnDock = PanelUnDock
  end
  object PanelRight: TPanel
    Left = 526
    Top = 0
    Width = 141
    Height = 373
    Align = alRight
    BevelOuter = bvNone
    DockSite = True
    ParentBackground = False
    TabOrder = 2
    Visible = False
    OnGetSiteInfo = FormGetSiteInfo
    OnUnDock = PanelUnDock
  end
  object MainMenu: TMainMenu
    Left = 248
    Top = 48
    object MenuItemFile: TMenuItem
      Caption = '&File'
      object MenuItemFileExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MenuItemFileExitClick
      end
    end
    object MenuItemView: TMenuItem
      Caption = '&View'
      object MenuItemFloatOnCloseDocked: TMenuItem
        Caption = 'Float on close docked'
        OnClick = MenuItemFloatOnCloseDockedClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuItemWhite: TMenuItem
        Caption = '&White'
        Hint = 'View white window'
        OnClick = MenuItemViewFormClick
      end
      object MenuItemBlue: TMenuItem
        Tag = 1
        Caption = '&Blue'
        OnClick = MenuItemViewFormClick
      end
      object MenuItemGreen: TMenuItem
        Tag = 2
        Caption = '&Green'
        OnClick = MenuItemViewFormClick
      end
      object MenuItemLime: TMenuItem
        Tag = 3
        Caption = '&Red'
        OnClick = MenuItemViewFormClick
      end
      object MenuItemPurple: TMenuItem
        Tag = 4
        Caption = '&Teal'
        OnClick = MenuItemViewFormClick
      end
      object MenuItemRed: TMenuItem
        Tag = 5
        Caption = '&Purple'
        OnClick = MenuItemViewFormClick
      end
      object MenuItemTeal: TMenuItem
        Tag = 6
        Caption = '&Lime'
        OnClick = MenuItemViewFormClick
      end
    end
  end
  object DelayedStartTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = DelayedStartTimerTimer
    Left = 328
    Top = 48
  end
end
