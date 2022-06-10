module Entity.Common where


class CopyValues a where
    copyValues :: a -> a -> a

class InitCreatedAt a where
    initCreatedAt :: a -> IO a