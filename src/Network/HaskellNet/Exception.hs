{-# LANGUAGE GADTs #-}
module Network.HaskellNet.Exception
  ( HaskellNetException(..)
  , ClientError(..)
  , ServerError(..)
  , AuthenticationFailed(..)
  , CommandError(..)
  , UnexpectedResponse(..)
  )
where

import Control.Exception

import Data.Typeable

-- | The superclass of all HaskellNet exceptions.
data HaskellNetException where
  HaskellNetException :: Exception e => e -> HaskellNetException
  deriving (Typeable)

instance Show HaskellNetException where
  show (HaskellNetException e) = show e

instance Exception HaskellNetException where
  displayException (HaskellNetException e) = show e

toHaskellNetException :: Exception e => e -> SomeException
toHaskellNetException = toException . HaskellNetException

fromHaskellNetException :: Exception e => SomeException -> Maybe e
fromHaskellNetException ex = do
  HaskellNetException e <- fromException ex
  cast e

--------------------------------------------------------------------------------

data AuthenticationFailed = AuthenticationFailed String
  deriving (Typeable)

instance Show AuthenticationFailed where
  show (AuthenticationFailed why) = "AuthenticationFailed: " <> why

instance Exception AuthenticationFailed where
  toException = toHaskellNetException
  fromException = fromHaskellNetException

--------------------------------------------------------------------------------

-- | Errors caused by the user, such as supplying an empty list of recipients.
data ClientError = ClientError String
  deriving (Typeable, Show)

instance Exception ClientError where
  toException = toHaskellNetException
  fromException = fromHaskellNetException

--------------------------------------------------------------------------------

-- | Erraneous server behavior.
data ServerError = ServerError String
  deriving (Typeable, Show)

instance Exception ServerError where
  toException = toHaskellNetException
  fromException = fromHaskellNetException

serverError :: String -> IO a
serverError = throwIO . ServerError

--------------------------------------------------------------------------------

data UnexpectedResponse = UnexpectedResponse String Int Int String
  deriving Typeable

instance Show UnexpectedResponse where
  show (UnexpectedResponse failedTo expected actual msg) = concat
    [ "UnexpectedResponse: "
    , failedTo
    , " expected ", show expected
    , ", actual response ", show actual
    , "(", msg , ")"
    ]
instance Exception UnexpectedResponse where
  toException = toHaskellNetException
  fromException = fromHaskellNetException

--------------------------------------------------------------------------------

-- | An error returned by the server after sending an IMAP/POP3/SMTP command.
data CommandError = CommandError String
  deriving (Typeable, Show)

instance Exception CommandError where
  toException = toHaskellNetException
  fromException = fromHaskellNetException

--------------------------------------------------------------------------------

-- | Some other error.
data OtherError = OtherError String
  deriving (Typeable, Show)

instance Exception OtherError where
  toException = toHaskellNetException
  fromException = fromHaskellNetException
