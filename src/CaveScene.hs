module CaveScene where

import           Arena
import           Control.Lens
import           Control.Monad.State       (State, evalState, execState, when)
import           DatasTypesClasses
import           Demo
import qualified Graphics.Rendering.OpenGL as GL
import           Mobs
import           Node
import           Scene                     (getSceneById)
import           Sprite
import           System.Random
import           Texture                   (createTexture, getTextureSize)
import           Transform

caveScene :: GL.Color4 Float -> IO Scene
caveScene clearColor = do
  GL.clearColor GL.$= clearColor
  back <- backNode
  front <- frontNode
  mob <- mobNode
  sword <- pseudoNode
  return Scene_ {_sceneChildren = [back, front, mob, sword], _sceneId = "caveScene"}

backSprite :: IO Sprite
backSprite = createSpriteWithPath "assets/sprites/png_pictures/cave/cave_back.png"

frontSprite :: IO Sprite
frontSprite = createSpriteWithPath "assets/sprites/png_pictures/cave/cave_front.png"

backNode :: IO Node
backNode = do
  sprite <- backSprite
  return $ createNode $ do
    nodeSprite .= Just sprite
    nodeLocalTransform .= createTransform (transformPosition .= GL.Vector3 0 0 0)

frontNode :: IO Node
frontNode = do
  sprite <- frontSprite
  return $ createNode $ do
    nodeSprite .= Just sprite
    nodeLocalTransform .= createTransform (transformPosition .= GL.Vector3 0 0 0.9)

mobNode :: IO Node
mobNode = do
  target1 <- targetNode
  target2 <- targetNode
  target3 <- targetNode
  let target1Upd = set (nodeLocalTransform . transformPosition) (GL.Vector3 10 6 0.6) target1
      target2Upd = set (nodeLocalTransform . transformPosition) (GL.Vector3 9 (-10) 0.6) target2
      target3Upd = set (nodeLocalTransform . transformPosition) (GL.Vector3 (-2) (-3) 0.6) target3
  return $ createNode $ do
    nodeLocalTransform .=
      createTransform
        (do transformPosition .= GL.Vector3 64 50 0.5
            transformAnchor .= (0.5, 0.5))
    nodeGameStateUpdate .= mobGameStateUpdate
    nodeUpdate .= mobNodeUpdate
    nodeChildren .= [return target1Upd, return target2Upd, return target3Upd, mobHPBarBlackNode]

mobNodeUpdate :: Node -> GameState -> Node
mobNodeUpdate node gameState =
  case node ^. nodeSprite of
    Nothing -> execState (updateMob gameState) node
    Just _  -> node
  where
    updateMob :: GameState -> State Node ()
    updateMob gameState =
      let currentMob = gameState ^. gameInfo . gameInfoCurrentFight
       in case currentMob of
            Nothing -> return ()
            Just (mob, _) ->
              let randomInt = mob ^. mobRandomSeed
                  sprite = (mob ^. mobInfo . mobInfoSprite)
               in do nodeSprite .= Just sprite
                     val <-
                       nodeLocalTransform . transformPosition <%= \(GL.Vector3 x y z) ->
                         GL.Vector3 (x + fromIntegral randomInt) y z
                     nodeLocalTransform . transformPosition .= val
                     nodeLocalTransform . transformSize .= sprite ^. spriteSize

mobGameStateUpdate :: Node -> (GameState -> GameState)
mobGameStateUpdate node =
  \gameState ->
    case gameState ^. gameInfo . gameInfoCurrentFight of
      Just _  -> gameState
      Nothing -> execState (setMob gameState) gameState
  where
    setMob :: GameState -> State GameState ()
    setMob gameState =
      let (seed, g) = randomR (-35, 35) (gameState ^. randomGenerator)
       in case gameState ^. gameInfo . gameInfoCurrentFight of
            Nothing -> do
              gameInfo . gameInfoCurrentFight .=
                let currentMobInfo = head (gameState ^. gameInfo . gameInfoMobsList)
                    playerInfo = gameState ^. gameInfo . gameInfoPlayerInfo
                 in Just (createMob currentMobInfo seed, createPlayer playerInfo)
              randomGenerator .= g
            Just _ -> return ()

createMob :: MobInfo -> Int -> Mob
createMob currentMobInfo seed =
  Mob
    { _mobInfo = currentMobInfo
    , _mobCurrentHP = fromIntegral $ currentMobInfo ^. mobInfoHp
    , _mobAtackCooldown = currentMobInfo ^. mobInfoAtackCooldown
    , _mobRandomSeed = seed
    }

createPlayer :: PlayerInfo -> Player
createPlayer playerInfo = Player {_playerAtackCooldown = 0, _playerCurrentHP = playerInfo ^. playerInfoHP}

targetSprite :: IO Sprite
targetSprite = createSpriteWithPath "assets/sprites/png_pictures/cave/target.png"

targetNode :: IO Node
targetNode = do
  sprite <- targetSprite
  return $ createNode $ do
    nodeSprite .= Just sprite
    nodeLocalTransform . transformSize .= sprite ^. spriteSize
    nodeLocalTransform . transformAnchor .= (0.5, 0.5)
    nodeGameStateUpdate .= targetGameStateUpdate
    nodeCollider .=
      let GL.Size w _ = sprite ^. spriteSize
       in Just $ CircleCollider (fromIntegral w / 2)
    nodeUpdate .= targetNodeUpdate

targetNodeUpdate :: Node -> GameState -> Node
targetNodeUpdate node gameState =
  let (x, y) = (gameState ^. stateMouseX, gameState ^. stateMouseY)
   in if isInCollision node x y
        then execState (nodeLocalTransform . transformScale .= (1.3, 1.3)) node
        else execState (nodeLocalTransform . transformScale .= (1, 1)) node

targetGameStateUpdate :: Node -> (GameState -> GameState)
targetGameStateUpdate node = checkClick
  where
    checkClick :: GameState -> GameState
    checkClick gameState =
      let (x, y) = (gameState ^. stateMouseX, gameState ^. stateMouseY)
       in if isInCollision node x y && gameState ^. stateMouseClick
            then checkHP $ hitMob gameState
            else gameState
    hitMob :: GameState -> GameState
    hitMob gameState =
      case gameState ^. gameInfo . gameInfoCurrentFight of
        Just (mob, player) ->
          if player ^. playerAtackCooldown == 0
            then execState
                   (do let (seed, g) =
                             randomR
                               (0, gameState ^. gameInfo . gameInfoPlayerInfo . playerInfoAtk)
                               (gameState ^. randomGenerator)
                       gameInfo . gameInfoCurrentFight .=
                         Just
                           ( mob {_mobCurrentHP = (mob & _mobCurrentHP) - fromIntegral seed}
                           , player
                               { _playerAtackCooldown =
                                   gameState ^. gameInfo . gameInfoPlayerInfo . playerInfoAtackCooldown
                               })
                       randomGenerator .= g)
                   gameState
            else gameState
    checkHP :: GameState -> GameState
    checkHP gameState =
      let mobM = gameState ^. gameInfo . gameInfoCurrentFight
       in case mobM of
            Nothing -> gameState
            Just (mob, _) ->
              if (mob ^. mobCurrentHP) <= 0
                then execState (switchScene gameState) gameState
                else gameState
    switchScene :: GameState -> State GameState ()
    switchScene gameState = do
      activeScene .= getSceneById gameState "firstScene"
      gameInfo . gameInfoCurrentFight .= Nothing

mobHPBarBlack :: IO Sprite
mobHPBarBlack = createSpriteWithPath "assets/sprites/png_pictures/cave/hp_mob_black.png"

mobHPBarBlackNode :: IO Node
mobHPBarBlackNode = do
  sprite <- mobHPBarBlack
  return $ createNode $ do
    nodeLocalTransform .=
      createTransform
        (do transformPosition .= GL.Vector3 0 (-20) 1
            transformAnchor .= (0.5, 0.5)
            transformSize .= sprite ^. spriteSize)
    nodeSprite .= Just sprite
    nodeChildren .= [mobHPBarRedNode]

mobHPBarRed :: IO Sprite
mobHPBarRed = createSpriteWithPath "assets/sprites/png_pictures/cave/hp_mob_red.png"

mobHPBarRedNode :: IO Node
mobHPBarRedNode = do
  sprite <- mobHPBarRed
  let (GL.Size w _) = sprite ^. spriteSize
  return $ createNode $ do
    nodeLocalTransform .=
      createTransform
        (do transformPosition .= GL.Vector3 (fromIntegral (-w) / 2) 0 0.91
            transformAnchor .= (0, 0.5)
            transformSize .= sprite ^. spriteSize)
    nodeSprite .= Just sprite
    nodeUpdate .= updateMobHPBar

updateMobHPBar :: Node -> GameState -> Node
updateMobHPBar node gameState =
  case gameState ^. gameInfo . gameInfoCurrentFight of
    Just (mob, _) ->
      set
        (nodeLocalTransform . transformScale)
        ((mob ^. mobCurrentHP) / fromIntegral (mob ^. mobInfo . mobInfoHp), 1)
        node
    Nothing -> node

swordAnimationSprite :: IO Sprite
swordAnimationSprite = createSpriteWithPath "assets/sprites/png_pictures/cave/sword.png"

swordAnimationNode :: IO Node
swordAnimationNode = do
  sprite <- swordAnimationSprite
  return $ createNode $ do
    nodeSprite .= Just sprite
    nodeLocalTransform .=
      createTransform
        (do transformAnchor .= (0.5, 1)
            transformSize .= sprite ^. spriteSize
            transformPosition .= GL.Vector3 0 (-30) 1)

pseudoNode :: IO Node
pseudoNode = do
  return $ createNode $ do
    nodeChildren .= [swordAnimationNode]
    nodeLocalTransform .= createTransform (do transformPosition .= GL.Vector3 64 (72 + 30) 0)
    nodeUpdate .= pseudoNodeUpdate
    nodeGameStateUpdate .= pseudoNodeGameStateUpdate

pseudoNodeUpdate :: Node -> GameState -> Node
pseudoNodeUpdate node gameState = execState rotate node
  where
    rotate =
      let fight = gameState ^. gameInfo . gameInfoCurrentFight
       in case fight of
            Just (_, player) -> do
              nodeLocalTransform . transformRotation .= 180 * (fromIntegral (player ^. playerAtackCooldown)) /
                (fromIntegral (gameState ^. gameInfo . gameInfoPlayerInfo . playerInfoAtackCooldown)) -
                90
            Nothing -> return ()

pseudoNodeGameStateUpdate :: Node -> GameState -> GameState
pseudoNodeGameStateUpdate node gameState = execState subCooldown gameState
  where
    subCooldown =
      let fight = gameState ^. gameInfo . gameInfoCurrentFight
       in case fight of
            Just (mob, player) -> do
              gameInfo . gameInfoCurrentFight .=
                let cooldown = player ^. playerAtackCooldown
                 in if cooldown > 0
                      then Just (mob, player {_playerAtackCooldown = cooldown - 1})
                      else Just (mob, player)
            Nothing -> return ()
