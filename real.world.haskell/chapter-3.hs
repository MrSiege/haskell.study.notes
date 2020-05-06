-- 定义新的数据类型，BookInfo 为类型构造器，Book 为值构造器
data BookInfo = Book Int String [String] 
                deriving (Show)

-- 定义类型别名，为了提高可读性而存在
type Books = [BookInfo]
type BookName = String

-- 代数数据类型，可以有多个值构造器
data Method = String | Int

myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

--参数化类型
-- data Maybe a = Just a | Nothing

-- 递归类型，引用自身
data List a = Cons a (List a) | Nil
              deriving (Show)

formList :: [a] -> List a
formList (x:xs) = Cons x (formList xs)
formList _      = Nil

-- 实现二叉树类型
-- data Tree a = Node a (Tree a) (Tree a)
--             | Empty
--               deriving (Show)

-- 练习１．实现一个与 formList 作用相反的函数，输入一个 List a，返回一个 [a]
isNil :: List a -> Bool
isNil Nil = True
isNil (Cons a b) = False

toList (Cons a b) = if isNil b then [a] else a : toList b

-- toList (formList "abc")
-- toList (formList [1, 2, 3])

-- 练习２．请仿造 Java 示例，定义一种只需要一个构造器的树类型。不要使用 Empty 构造器，而是用 Maybe 表示节点的子节点。

data BinaryTree a = BinaryTree {
  value :: a,
  left  :: Maybe (BinaryTree a),
  right :: Maybe (BinaryTree a)
} deriving(Show)

peopleNetwork = BinaryTree {
  value = "louyue",
  left  = Just (BinaryTree "shihuang" Nothing Nothing),
  right = Just (BinaryTree "Aliz" Nothing Nothing)
}

-- 报告错误
-- 使用条件判断
-- second :: [a] -> a
-- second xs = if null (tail xs)
--             then error "list too short"
--             else head (tail xs)
-- 使用模式匹配
second :: [a] -> Maybe a
second (_:x:_) = Just x
second [] = Nothing

-- 引入局部变量，let 标示一个变量声明区块的开始，in 标示结束，in之后的表达式可以使用 let 中定义的变量
lend amount balance = let reserve = 100
                          newBalance = balance - amount --这里不会计算，只是绑定表达式到 newBalance
                      in if balance < reserve
                          then Nothing
                          else Just newBalance

lend1 amount balance = if balance < reserve
                        then Nothing
                        else Just newBalance
                        where reserve = 100
                              newBalance = balance - amount

-- case 表达式
fromMaybe defval wrapped = 
  case wrapped of --模式匹配　wrapped　为 Nothing 或 Just
    Nothing    -> defval
    Just value -> value

--　使用守卫实现条件求值
nodeSame (BinaryTree a _ _) (BinaryTree b _ _) | a == b = Just a
nodeSame _ _ = Nothing

isSame = nodeSame (BinaryTree 1 Nothing Nothing) (BinaryTree 2 Nothing Nothing)

-- 练习1.　写一个函数，用来计算一个列表元素的个数．出于测试要求，保证其输出的结果和标准函数 length 保持一致．
-- 练习2.　添加函数的类型签名于你的源文件．出于测试要求，再次加载源文件到ghci．
myLength :: [a] -> Int
myLength (_: xs) = 1 + myLength xs
myLength _       = 0

-- 练习3.　写一个函数，用来计算列表的平均值，即，列表元素的总和除以列表的长度．
-- 你可能需要用到 fromIntegral 函数将列表长度变量从 integer 类型到 float 类型进行转换.
sumList :: [Double] -> Double
sumList (x: xs) = x + sumList xs
sumList _       = 0

average :: [Double] -> Double
average xs = sumList xs / fromIntegral (myLength xs)

-- 练习4. 将一个列表变成回文序列，即，他应该读起来完全一样，不管是从前往后还是从后往前．
-- 举个例子，考虑一个列表 [1,2,3]，你的函数应该返回 [1,2,3,3,2,1]．
myLast :: [a] -> a
myLast (x: xs) = if myLength xs > 0 then myLast xs else x
myLast _       = error "list too short"

dropLast :: [a] -> [a]
dropLast xs = take (length xs - 1) xs

myReverse :: [a] -> [a]
myReverse (x: xs) = myLast (x: xs): myReverse (dropLast (x: xs))
myReverse _       = []

palindrome :: [a] -> [a]
palindrome xs = xs ++ myReverse xs

-- 练习5. 写一个函数，用来确定他的输入是否是一个回文序列
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- 练习6. 创造一个函数，用于排序一个包含许多列表的列表，其排序规则基于他的子列表的长度．（你可能要看看 Data.List 模块的 sortBy 函数．）
mySortBy :: [[a]] -> [[a]]
mySortBy (x: xs) = myLength