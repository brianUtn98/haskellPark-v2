module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Montaña rusa" $ do
    describe "Montaña rusa velocidad mayor a 50" $ do
      it "Montaña rusa aumenta el nivel de emocion" $ do
        nivelEmocion (montañaRusa 55 40 juan) `shouldBe` 78.25
    describe "Montaña rusa velocidad menor a 50" $ do
      it "Montaña rusa disminuye la emocion" $ do
        nivelEmocion (montañaRusa 45 40 juan) `shouldBe` 28.5
      it "Montaña rusa disminuye la satisfaccion" $ do
        nivelSatisfaccion (montañaRusa 45 40 juan) `shouldBe` 18
  describe "Caida libre" $ do
    it "Caida libre aumenta la emocion de una persona" $ do
      nivelEmocion (caidaLibre 10 juan) `shouldBe` 32
  describe "Mundo maya" $ do
    it "Mundo maya aumenta el nivel de emocion" $ do
      nivelEmocion (mundoMaya ana) `shouldBe` 22
    it "Mundo maya aumenta el nivel de cultura" $ do
      nivelCultura (mundoMaya ana) `shouldBe` 72
  describe "Show de magia" $ do
    it "Show de magia aumenta el nivel de satisfaccion si el nivel de cultura es mayor a 50" $ do
      nivelSatisfaccion (showDeMagia ana) `shouldBe` 30
    it "Show de magia aumenta el nivel de emocion si el nivel de cultura es menor o igual a 50" $ do
      nivelEmocion (showDeMagia juan) `shouldBe` 60

  describe "Salto bungee" $ do
    it "Salto bungee equivale a una caida libre de 20 metros luego de una montaña rusa de 200 de velocidad y 10 de altura" $ do
      nivelEmocion (saltoBungee juan) `shouldBe` nivelEmocion (caidaLibre 20 (montañaRusa 200 10 juan))
  describe "Zombie walk" $ do
    it "Zombie walk equivale a show de magia luego de una montaña rusa de 100 de velocidad y 50 de altura" $ do
      nivelEmocion (zombieWalk ana) `shouldBe` nivelEmocion (showDeMagia (montañaRusa 100 50 ana))