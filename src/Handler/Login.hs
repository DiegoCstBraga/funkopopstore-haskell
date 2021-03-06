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
                           <p>
                              Logado como: #{email}
                           <form method=post action=@{SairR}>
                              <input class="aButton" type="submit" value="Sair">
                     $nothing
                        <a class="aButton" href=@{UsuarioR}>
                           Criar Conta 
                  
                        <a class="aButton" href=@{EntrarR}>
                           Entrar
                     
                     <a class="aButton" href=@{ListProdR}>
                        Listar Funkos 

                     <a class="aButton" href=@{ListCompraR}>
                        Minhas Compras 

            <main>
               <h2>
                  LOGIN PAGE
      |]
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

          main {
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
         }
         main img {
            height: 394px;
            width: auto;
         }
      |]

  [whamlet|


      <body>
         <header>
            <div class="esquerda">
               <a class="home" href=@{AdminR}>
                  <img src="https://i.imgur.com/c6K3Xyj.png" alt="logo">

            <div>
               <nav class="direita">
                  $maybe email <- sess
                     <div class="conta">
                        <p>
                           Logado como: #{email}
                        <form method=post action=@{SairR}>
                           <input class="aButton" type="submit" value="Sair">
                  $nothing
                     <a class="aButton" href=@{UsuarioR}>
                        Criar Conta 
               
                     <a class="aButton" href=@{EntrarR}>
                        Entrar

                  <a class="aButton" href=@{ListProdAdmR}>
                     Listar Funkos
                  
                  <a class="aButton" href=@{ProdutoR}>
                     Cadastrar Funkos 


         <main>
            <h2>
               Página do Admin
            <img src="https://i.imgur.com/QgAlnIW.png" alt="AdminPage"> 
   |]
  toWidget
    [julius|

         alert("Seja bem vindo, Admin")
    |]
