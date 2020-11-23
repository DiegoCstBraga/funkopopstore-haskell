{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Usuario where

import Import
import Text.Lucius
import Tool

formUsu :: Form (Usuario, Text)
formUsu =
  renderBootstrap $
    (,)
      <$> ( Usuario
              <$> areq textField "Nome: " Nothing
              <*> areq emailField "E-mail: " Nothing
              <*> areq passwordField "Senha: " Nothing
          )
      <*> areq passwordField "Digite Novamente: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
  (widget, _) <- generateFormPost formUsu
  msg <- getMessage
  defaultLayout $ do
    toWidgetHead $(luciusFile "templates/form.lucius")
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    toWidgetHead
      [lucius|
         @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700;900&display=swap');

         * {
            font-family: 'Roboto', sans-serif;
         }

         body{
            margin: 0 auto;
         }

         header { 
            display: flex;
            flex-direction: row;
            justify-content: space-between;
            align-items: center;
            background-color: #202020;
            padding: 1rem 0 1rem 1rem;
         }

         header img {
            width: auto;
            height: 100px;
         }

         .direita{
            display: flex;
            flex-direction: row;

            margin-right: 1rem;

            color: #f0f0f0;
            font-size: 2rem;

         }

         .conta{
            display: flex;
            flex-direction: row;
            align-items: center;         

         }

         header a, p{
            margin: 0 1rem 0 0;
            font-size: 2rem;
            color: #f0f0f0
         }

         input{
            color: #000;
            margin: 1rem 0;
            padding: 0.5rem;
            border-radius: 0.5rem;
            background-color: #cfcfcf;
            border: hidden;
            outline: none;
         }

         .aButton{
            margin: 0 1rem 0 0;
            font-size: 2rem;
            padding: 0.5rem;
            color: #f0f0f0;
            border-radius: 0.5rem;
            background-color: #f64668;
            border: hidden;
            outline: none;
         }
         
      |]
    [whamlet|

         <body>
            <header>
               <div class="esquerda">
                  <a class="home" href=@{HomeR}>
                     <img src="https://i.imgur.com/c6K3Xyj.png" alt="logo">

               <div>
                  <nav class="direita">
                     $maybe email <- sess
                        <div class="conta">
                           <p">
                              Logado como: #{email}
                           <form method=post action=@{SairR}>
                              <input class="aButton" type="submit" value="Sair">
                     $nothing
                        <a class="aButton" href=@{UsuarioR}>
                           Criar Conta 
                  
                        <a class="aButton" href=@{EntrarR}>
                           Entrar

                     <a class="aButton" href=@{ListProdR}>
                        Ver Funkos 

                     <a class="aButton" href=@{ListCompraR}>
                        Minhas Compras 

            <main>
               <h2>
                  CADASTRO PAGE 
      |]

    geraForm UsuarioR "CADASTRO DE USUARIO" "Cadastrar" msg widget

postUsuarioR :: Handler Html
postUsuarioR = do
  ((result, _), _) <- runFormPost formUsu
  case result of
    FormSuccess (usuario, veri) -> do
      if (usuarioSenha usuario == veri)
        then do
          runDB $ insert400 usuario
          setMessage
            [shamlet|
                    <div>
                        USUARIO INCLUIDO
                |]
          redirect UsuarioR
        else do
          setMessage
            [shamlet|
                    <div>
                        SENHAS NÃO CONFEREM
                |]
          redirect UsuarioR
    _ -> redirect HomeR
