inherited TabDockHost: TTabDockHost
  Left = 412
  Top = 306
  Caption = 'TabDockHost'
  ClientHeight = 275
  ClientWidth = 492
  PixelsPerInch = 96
  TextHeight = 13
  inherited Memo: TMemo
    Width = 488
    Height = 271
    TabOrder = 1
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 492
    Height = 275
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    DockSite = True
    TabOrder = 0
    OnDockOver = PageControlDockOver
    OnGetSiteInfo = PageControlGetSiteInfo
    OnUnDock = PageControlUnDock
  end
end
