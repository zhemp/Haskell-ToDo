-- appEvent appState (T.VtyEvent e) =  
--     let index = pointer appState
--         l = getList index appState
--     in 
--         case e of
--             V.EvKey (V.KChar '+') [] ->
--                 let 
--                     maxId = getMaxId index appState
--                     el = createMainTask index (maxId+1) "this is my new String"
--                     in 
--                     case l^.(L.listSelectedL) of
--                         Just pos ->
--                                 M.continue $ insertState index (L.listMoveTo (pos + 1) $ L.listInsert (pos + 1) el l) (setMaxId index appState (maxId + 1))
--                         Nothing ->
--                                 M.continue $ insertState index (L.listMoveTo 1 $ L.listInsert 0 el l) (setMaxId index appState (maxId + 1))

--             V.EvKey (V.KChar '-') [] ->
--                 case L.listSelectedElement l of
--                     Nothing -> M.continue appState
--                     Just (pos,task)  -> 
--                         if isSub task then 
--                             let
--                                 updatedList = L.listRemove pos l
--                             in
--                                 M.continue $ insertState index updatedList appState        
--                         else M.continue appState  



                    -- V.EvKey (V.KChar '+') [] ->
                    --     let 
                    --         maxId = getMaxId index appState
                    --         el = createMainTask index (maxId+1) "this is my new String"
                    --         in 
                    --         case l^.(L.listSelectedL) of
                    --             Just pos ->
                    --                     M.continue $ insertState index (L.listMoveTo (pos + 1) $ L.listInsert (pos + 1) el l) (setMaxId index appState (maxId + 1))
                    --             Nothing ->
                    --                     M.continue $ insertState index (L.listMoveTo 1 $ L.listInsert 0 el l) (setMaxId index appState (maxId + 1))
                    