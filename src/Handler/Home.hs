{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import Database.Persist.Postgresql
import Import

--         <img src=@{StaticR img_produto_jpg}>
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  addStylesheet (StaticR css_bootstrap_css)
  toWidget
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
         margin-right: 1rem;
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
  sess <- lookupSession "_EMAIL"
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
                        <p style="margin=0 1rem 0 0;">
                           Logado como: #{email}
                        <form method=post action=@{SairR}>
                           <input class="aButton" type="submit" value="Sair">

                        <a class="aButton" href=@{ListProdR}>
                           Ver Funkos 

                        <a class="aButton" href=@{ListCompraR}>
                           Minhas Compras 
                  $nothing
                     <a class="aButton" href=@{UsuarioR}>
                        Criar Conta 
               
                     <a class="aButton" href=@{EntrarR}>
                        Entrar

                     <a class="aButton" href=@{ProdutoR}>
                        Cadastrar Produtos
                  

         <main>
            <h2>
               HOME PAGE    
                    
    |]
