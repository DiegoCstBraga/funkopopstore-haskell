{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Login where

import Import
import Text.Lucius
import Tool

formLogin :: Form (Text, Text)
formLogin =
  renderBootstrap $
    (,)
      <$> areq emailField "E-mail: " Nothing
      <*> areq passwordField "Senha: " Nothing

getEntrarR :: Handler Html
getEntrarR = do
  (widget, _) <- generateFormPost formLogin
  msg <- getMessage
  defaultLayout $ do
    geraForm EntrarR "ENTRAR" "Login" msg widget

postEntrarR :: Handler Html
postEntrarR = do
  ((result, _), _) <- runFormPost formLogin
  case result of
    FormSuccess ("admin@admin.com", "root") -> do
      setSession "_EMAIL" "admin@admin.com"
      redirect AdminR
    FormSuccess (email, senha) -> do
      -- select * from usuario where email=digitado.email
      usuario <- runDB $ getBy (UniqueEmail email)
      case usuario of
        Nothing -> do
          setMessage
            [shamlet|

                        <div>
                            E-mail não foi encontrado!
                    |]
          redirect EntrarR
        Just (Entity _ usu) -> do
          if (usuarioSenha usu == senha)
            then do
              setSession "_EMAIL" (usuarioEmail usu)
              redirect HomeR
            else do
              setMessage
                [shamlet|

                            <div>
                                Senha incorreta!
                        |]
              redirect EntrarR
    _ -> redirect HomeR

postSairR :: Handler Html
postSairR = do
  deleteSession "_EMAIL"
  redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout $ do
  [whamlet|

            <h1>
                BEM-VINDO ADMIN
    |]
