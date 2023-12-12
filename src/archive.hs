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
                    

                    -- -- complete the input, and add it to the list
                    -- let 
                    --     el = createMainTask index (maxId+1) input 
                    --     maxId = getMaxId index appState
                    -- in
                    --     case l^.(L.listSelectedL) of
                    --         Just pos ->
                    --                 M.continue $ setInputField Nothing $ insertState index (L.listMoveTo (pos + 1) $ L.listInsert (pos + 1) el l) (setMaxId index appState (maxId + 1))
                    --         Nothing ->
                    --                 M.continue $ setInputField Nothing $ insertState index (L.listMoveTo 1 $ L.listInsert 0 el l) (setMaxId index appState (maxId + 1))





                                        -- add the char to the inputField
                    -- M.continue $ appState { inputField = Just (input ++ [c]) }
                    -- try to visualize the inputField when receiving input, using ListInsert
                    -- let 
                    --     el = createMainTask index maxId (input ++ [c])
                    --     maxId = getMaxId index appState
                    -- in
                    --     case l^.(L.listSelectedL) of
                    --         Just pos ->
                    --                 M.continue $ setInputField (Just (input ++ [c])) $ insertState index (L.listMoveTo (pos + 1)  $ L.listInsert pos el $ L.listRemove pos l) appState
                    --         Nothing ->
                    --                 M.continue $ setInputField (Just (input ++ [c])) $ insertState index (L.listMoveTo (0 + 1) $ L.listInsert 0 el $ L.listRemove 0 l) appState


                                            -- let 
                        --     maxId = getMaxId index appState
                        --     el = createMainTask index (maxId+1) ""
                        --     in 
                            -- case L.listSelectedElement l of
                            --     Just (pos,task) ->
                            --         let newPos = pos + getTaskLen l (getTaskId task)
                            --         in
                            --             M.continue $ setInputField (Just "") appstate
                            --     Nothing ->
                            --             M.continue $ setInputField (Just "") appstate
                        -- let 
                        --     maxId = getMaxId index appState
                        --     el = createMainTask index (maxId+1) ""
                        --     in 
                            -- case L.listSelectedElement l of
                            --     Just (pos,task) ->
                            --         let newPos = getTaskEndIndex l (getTaskId task)
                            --         in
                            --             M.continue $ setInputField (Just "") appstate
                            --     Nothing ->
                            --             M.continue $ setInputField (Just "") appstate
                                                -- case L.listSelectedElement l of
                        --     Nothing -> M.continue appState
                        --     Just (pos, task) ->
                        --         let 
                        --             idx = getTaskId task
                        --             el = createSubTask idx ""
                        --             in 
                        --             case l^.(L.listSelectedL) of
                        --                 Just pos ->
                        --                         M.continue $ setInputField (Just "") $ insertState index (L.listMoveTo (pos + 1) $ L.listInsert (pos + 1) el l) appState
                        --                 Nothing ->
                        --                         M.continue $ setInputField (Just "") $ insertState index (L.listMoveTo 1 $ L.listInsert 0 el l) appState
                                        -- V.EvKey (V.KChar '-') [] ->
                    --     case l^.(L.listSelectedL) of
                    --         Nothing -> M.continue appState
                    --         Just pos  -> 
                    --             let
                    --                 updatedList = L.listRemove pos l
                    --             in
                    --                 M.continue $ insertState index updatedList appState  