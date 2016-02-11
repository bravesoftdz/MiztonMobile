object DMConnection: TDMConnection
  OldCreateOrder = False
  OnDestroy = DataModuleDestroy
  Height = 294
  Width = 345
  object UniConnection1: TUniConnection
    ProviderName = 'MySQL'
    Port = 3306
    Database = 'wiconetc_bd_one_go'
    Username = 'wiconetc_ewan'
    Server = 'wiconet.com.mx'
    Connected = True
    LoginPrompt = False
    Left = 40
    Top = 38
    EncryptedPassword = '93FF88FFC8FF9EFFABFF99FFCAFFC6FF88FFCBFF'
  end
end
