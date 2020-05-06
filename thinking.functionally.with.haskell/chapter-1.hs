import Data.Char

-- 例子：高频词，给定一段文本，计算出每个词的出现频率，并且按降序排序
-- 类型同义词 type synonyms
type Text = [Char]

-- 将文本分解为词的列表
-- words :: Text -> [Word]
-- words v = [v]

-- 转换大写字母为小写
myToLower :: Char -> Char
myToLower v = if (ord v >= 65) && (ord v <= 96)
  then chr ((ord v) + (ord 'a' - ord 'A'))
  else v


-- 将词的列表按字典排序
-- sortWords :: [Word] -> [Word]

-- 计算在有序列表中每个词连续出现的次数
-- countRuns :: [Word] -> [(Int, Word)]

-- 按照词出现次数从大到小排序
-- sortRuns :: [(Int, Word)] -> [(Int, Word)]

-- 取得数组中的前 n 个元素
-- take :: Int -> [a] -> [a]

-- 将列表中的每一个元素映射为一个字符串
-- showRun :: (Int, Word) -> String

-- 将列表中的所有元素串联为一个字符串
-- concat :: [String] -> String

-- 最后的解为
-- commonWords :: Int -> Text -> String

-- commonWords n = concat · sortRuns ·sortRuns · countRuns · sortWords · words · toLower