object DockableForm: TDockableForm
  Left = 117
  Top = 294
  Anchors = [akLeft]
  BorderStyle = bsToolWindow
  Caption = 'DockableForm'
  ClientHeight = 217
  ClientWidth = 391
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDockOver = FormDockOver
  OnShow = FormShow
  OnStartDock = FormStartDock
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    AlignWithMargins = True
    Left = 2
    Top = 2
    Width = 387
    Height = 213
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
end
