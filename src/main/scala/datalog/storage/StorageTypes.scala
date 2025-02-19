package datalog.storage

import datalog.dsl.{Constant, Term, Variable}
import datalog.dsl.Variable

import scala.collection.mutable

type RelationId = Int
type KnowledgeId = Int
enum DB:
  case Derived, Delta
enum KNOWLEDGE:
  case New, Known

/* TODO: expand to other types, right now we just support Variables + Constants that are either Int or String.
 * When expended, may want the DSL types and the storage types to diverge, so use type aliases (for now) instead of DSL types. */
type StorageTerm = Term
type StorageVariable = Variable
type StorageConstant = Constant

/* All methods used within storage managers for now, so methods only defined with precise types */
trait Row[T]

/* Most methods used within storage manager so are defined on the implementation, other than length (execution) and factToString (printer) */
trait Relation[T] extends IterableOnce[Row[T]] {
  def length: Int
  def factToString: String
}

/* EDBs always operate on StorageTerms only */
type EDB = Relation[StorageTerm]

/* All but toSeq can be defined on the precise type */
trait Database[T <: EDB] {
  def toSeq: Seq[(RelationId, T)]
}

/**
 * Quick BiMap for namespaces
 */
class NS() {
  private val nameToRid = mutable.Map[String, RelationId]()
  private val rIdToName = mutable.Map[RelationId, String]()
  def apply(name: String): RelationId = nameToRid(name)
  def apply(rId: RelationId): String = rIdToName(rId)
  def update(key: String, value: RelationId): Unit = {
    nameToRid(key) = value
    rIdToName(value) = key
  }
  def update(key: RelationId, value: String): Unit = {
    rIdToName(key) = value
    nameToRid(value) = key
  }
  def contains(key: String): Boolean = nameToRid.contains(key)
  def contains(key: RelationId): Boolean = rIdToName.contains(key)
  def rIds(): Iterable[RelationId] = rIdToName.keys
}
