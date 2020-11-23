{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Compra where

import Database.Persist.Sql
import Import
import Tool

getListCompraR :: Handler Html
getListCompraR = do
  sess <- lookupSession "_EMAIL"
  case sess of
    Nothing -> redirect HomeR
    Just email -> do
      usu <- runDB $ getBy (UniqueEmail email)
      case usu of
        Nothing -> redirect HomeR
        Just (Entity uid usuario) -> do
          let sql =
                "SELECT ??,??,?? FROM usuario \
                \ INNER JOIN compra ON compra.usuarioid = usuario.id \
                \ INNER JOIN produto ON compra.produtoid = produto.id \
                \ WHERE usuario.id = ?"
          produtos <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario, Entity Compra, Entity Produto)]
          defaultLayout $ do
            addStylesheet (StaticR css_bootstrap_css)
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

                  main li {
                     font-size: 2rem;
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
                              Ver Funkos 

                           <a class="aButton" href=@{ListCompraR}>
                              Minhas Compras 

                  <main>
                     <h1>
                        Compras de #{usuarioNome usuario}
                     
                     <ul>
                        $forall (Entity _ _, Entity _ compra, Entity _ produto) <- produtos
                              <li>
                                 #{produtoNome produto}: #{produtoPreco produto * (fromIntegral (compraQtunit compra))}
            |]

postComprarR :: ProdutoId -> Handler Html
postComprarR pid = do
  ((resp, _), _) <- runFormPost formQt
  case resp of
    FormSuccess qt -> do
      sess <- lookupSession "_EMAIL"
      case sess of
        Nothing -> redirect HomeR
        Just email -> do
          usuario <- runDB $ getBy (UniqueEmail email)
          case usuario of
            Nothing -> redirect HomeR
            Just (Entity uid _) -> do
              runDB $ insert (Compra pid uid qt)
              redirect ListCompraR
    _ -> redirect HomeR
