package net.defplus.trophygen
import scala.collection.mutable.ArrayBuffer

// スコアからランク(称号)を計算するのためのヘルプオブジェクト。
// Lvに達するためにLv*Lv*100のポイントを必要にしてある。
// ここをいじれば好きな称号をつけられるぞ！
object ScoreRank extends RankChecker[Int, String](0, "Lv1") {
  for (lv <- 2 to 100) {
    +=(lv * lv * 100, "Lv" + lv)
  }
  //　※参考資料　(0, "深海志願兵")
  //  +=(5000, "深海二等兵") += (10000, "深海一等兵") += (15000, "深海上等兵")
  //  +=(20000, "深海兵長") += (27000, "深海三等軍曹") += (34000, "深海二等軍曹")
  //  +=(41000, "深海一等軍曹") += (50000, "深海曹長") += (59000, "深海上級曹長")
  //  +=(70000, "深海専任曹長") += (83000, "深海准尉") += (98000, "深海上級准尉")
  //  +=(107000, "深海少尉") += (126000, "深海中尉") += (147000, "深海大尉")
  //  +=(160000, "深海少佐") += (185000, "深海中佐") += (212000, "深海大佐")
  //  +=(241000, "深海准将") += (275000, "深海少将") += (310000, "深海中将")
  //  +=(350000, "深海大将") += (395000, "深海軍元帥") += (445000, "深海政府大統領")
  //  +=(500000, "深海帝国皇帝") += (560000, "深海連邦総長") += (625000, "深海星界王")
  //  +=(700000, "深海銀河覇者") += (785000, "深海宇宙魔王") += (875000, "深海次元神")
  //  +=(1000000, "深海超越存在")
}

// RPGでよくある経験値とレベルの関係を表現するデータ構造。
// key:Tが経験値、value:Uがレベルに相当。
// コンストラクタに入れるのは初期レベル
class RankChecker[T, U](var key: T, var value: U) {

  val ranks = new ArrayBuffer[Rank[T, U]]

  def +=(key: T, value: U) = {
    ranks += new Rank[T, U](key, value)
    this
  }

  def check(comp: Comparable[T]): (U, T) = {
    var firsttime = true
    for (rank <- ranks) {
      if (rank.judge(comp)) {
        value = rank.value
      } else {
        if (firsttime) {
          key = rank.key
          firsttime = false
        }
      }
    }
    (value, key)
  }

  class Rank[V, M](val key: V, val value: M) {
    def judge(comp: Comparable[V]): Boolean = {
      comp.compareTo(this.key) >= 0
    }
  }
}