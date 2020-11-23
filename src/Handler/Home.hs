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
         color: #f0f0f0;
         font-size: 2rem;

         background-color: blue;
      }

      .conta{
         display: flex;
         flex-direction: row;
         align-items: center;

         color: #f0f0f0;
         font-size: 2rem;

         background-color: red;
      }

      header a{
         margin-right: 1rem;
         font-size: 2rem;
         color: #f0f0f0
      }

      input{
         color: #000;
      }

   |]
  sess <- lookupSession "_EMAIL"
  [whamlet|
      <body>
         <header>
            <div class="esquerda">
               <a class="home" href=@{HomeR}>
                  <img src="https://i.imgur.com/c6K3Xyj.png" alt="logo">

            <div >
               <nav class="direita">
                  $maybe email <- sess
                     <div class="conta">
                        <p>
                           Logado como: #{email}
                        <form method=post action=@{SairR}>
                           <input type="submit" value="Sair">
                  $nothing
                     <a href=@{UsuarioR}>
                        Criar Conta 
               
                     <a href=@{EntrarR}>
                        Entrar

                  <a href=@{ProdutoR}>
                     Listar Produtos

         <main>
            <h2>
               HOME PAGE

            
                    
    |]
