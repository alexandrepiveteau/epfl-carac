package test.examples.trains

import datalog.dsl.{Constant, Program, __}
import test.ExampleTestGenerator

import java.nio.file.Paths
class trains_test extends ExampleTestGenerator("trains") with trains
trait trains {
  val toSolve = "ReachableStops"
  def pretest(program: Program): Unit = {
    val ReachableStops = program.relation[Constant]("ReachableStops")
    val AdjacentStops = program.relation[Constant]("AdjacentStops")
    val Connected = program.relation[Constant]("Connected")

    AdjacentStops("Berowra","Mount Kuring-gai","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Mount Kuring-gai","Mount Colah","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Mount Colah","Asquith","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Asquith","Hornsby","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Hornsby","Waitara","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Waitara","Wahroonga","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Wahroonga","Warrawee","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Warrawee","Turramurra","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Turramurra","Pymble","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Pymble","Gordon","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Gordon","Killara","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Killara","Lindfield","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Lindfield","Roseville","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Hornsby","Normanhurst","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Normanhurst","Thornleigh","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Thornleigh","Pennant Hills","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Pennant Hills","Beecroft","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Beecroft","Cheltenham","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Cheltenham","Epping","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Epping","Macquarie University","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Macquarie University","Macquarie Park","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Macquarie Park","North Ryde","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Chatswood","Artarmon","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Artarmon","St Leonards","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("St Leonards","Wollstonecraft","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Wollstonecraft","Waverton","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Waverton","North Sydney","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("North Sydney","Milsons Point","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Milsons Point","Wynyard","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Wynyard","Town Hall","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Town Hall","Central","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Central","Redfern","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Redfern","Burwood","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Burwood","Strathfield","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("North Strathfield","Concord West","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Concord West","Rhodes","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Rhodes","Meadowbank","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Meadowbank","West Ryde","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("West Ryde","Denistone","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Denistone","Eastwood","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Eastwood","Epping","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Lidcombe","Auburn","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Auburn","Clyde","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Clyde","Granville","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Granville","Harris Park","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Harris Park","Parramatta","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Parramatta","Westmead","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Westmead","Wentworthville","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Wentworthville","Pendle Hill","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Pendle Hill","Toongabbie","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Toongabbie","Seven Hills","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Seven Hills","Blacktown","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Doonside","Rooty Hill","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Rooty Hill","Mount Druitt","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Mount Druitt","St Marys","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("St Marys","Werrington","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Werrington","Kingswood","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Kingswood","Penrith","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Penrith","Emu Plains","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Marayong","Quakers Hill","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Quakers Hill","Schofields","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Schofields","Riverstone","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Riverstone","Vineyard","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Vineyard","Mulgrave","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Mulgrave","Windsor","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Windsor","Clarendon","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Clarendon","East Richmond","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("East Richmond","Richmond","T1 North Shore, Northern & Western") :- ()
    AdjacentStops("Town Hall","Wynyard","T3 Bankstown Line") :- ()
    AdjacentStops("Wynyard","Circular Quay","T3 Bankstown Line") :- ()
    AdjacentStops("Circular Quay","St James","T3 Bankstown Line") :- ()
    AdjacentStops("St James","Museum","T3 Bankstown Line") :- ()
    AdjacentStops("Museum","Central","T3 Bankstown Line") :- ()
    AdjacentStops("Central","Redfern","T3 Bankstown Line") :- ()
    AdjacentStops("Redfern","Erskineville","T3 Bankstown Line") :- ()
    AdjacentStops("Erskineville","St Peters","T3 Bankstown Line") :- ()
    AdjacentStops("St Peters","Sydenham","T3 Bankstown Line") :- ()
    AdjacentStops("Sydenham","Marrickville","T3 Bankstown Line") :- ()
    AdjacentStops("Marrickville","Dulwich Hill","T3 Bankstown Line") :- ()
    AdjacentStops("Dulwich Hill","Hurlstone Park","T3 Bankstown Line") :- ()
    AdjacentStops("Hurlstone Park","Canterbury","T3 Bankstown Line") :- ()
    AdjacentStops("Canterbury","Campsie","T3 Bankstown Line") :- ()
    AdjacentStops("Campsie","Belmore","T3 Bankstown Line") :- ()
    AdjacentStops("Belmore","Lakemba","T3 Bankstown Line") :- ()
    AdjacentStops("Lakemba","Wiley Park","T3 Bankstown Line") :- ()
    AdjacentStops("Wiley Park","Punchbowl","T3 Bankstown Line") :- ()
    AdjacentStops("Punchbowl","Bankstown","T3 Bankstown Line") :- ()
    AdjacentStops("Bankstown","Yagoona","T3 Bankstown Line") :- ()
    AdjacentStops("Yagoona","Birrong","T3 Bankstown Line") :- ()
    AdjacentStops("Regents Park","Berala","T3 Bankstown Line") :- ()
    AdjacentStops("Berala","Lidcombe","T3 Bankstown Line") :- ()
    AdjacentStops("Sefton","Chester Hill","T3 Bankstown Line") :- ()
    AdjacentStops("Chester Hill","Leightonfield","T3 Bankstown Line") :- ()
    AdjacentStops("Leightonfield","Vilawood","T3 Bankstown Line") :- ()
    AdjacentStops("Vilawood","Carramar","T3 Bankstown Line") :- ()
    AdjacentStops("Carramar","Cabramatta","T3 Bankstown Line") :- ()
    AdjacentStops("Cabramatta","Warwick Farm","T3 Bankstown Line") :- ()
    AdjacentStops("Warwick Farm","Liverpool","T3 Bankstown Line") :- ()
    AdjacentStops("Liverpool","Town Hall","T3 Bankstown Line") :- ()
    AdjacentStops("Town Hall","Central","T3 Bankstown Line") :- ()
    AdjacentStops("Central","Redfern","T3 Bankstown Line") :- ()
    AdjacentStops("Redfern","Burwood","T3 Bankstown Line") :- ()
    AdjacentStops("Burwood","Strathfield","T3 Bankstown Line") :- ()
    AdjacentStops("North Strathfield","Concord West","T3 Bankstown Line") :- ()
    AdjacentStops("Concord West","Rhodes","T3 Bankstown Line") :- ()
    AdjacentStops("Rhodes","Meadowbank","T3 Bankstown Line") :- ()
    AdjacentStops("Meadowbank","West Ryde","T3 Bankstown Line") :- ()
    AdjacentStops("West Ryde","Denistone","T3 Bankstown Line") :- ()
    AdjacentStops("Denistone","Eastwood","T3 Bankstown Line") :- ()
    AdjacentStops("Eastwood","Epping(aboveground)","T3 Bankstown Line") :- ()
    AdjacentStops("Lidcombe","Auburn","T3 Bankstown Line") :- ()
    AdjacentStops("Auburn","Clyde","T3 Bankstown Line") :- ()
    AdjacentStops("Clyde","Granville","T3 Bankstown Line") :- ()
    AdjacentStops("Granville","Harris Park","T3 Bankstown Line") :- ()
    AdjacentStops("Harris Park","Parramatta","T3 Bankstown Line") :- ()
    AdjacentStops("Parramatta","Westmead","T3 Bankstown Line") :- ()
    AdjacentStops("Westmead","Wentworthville","T3 Bankstown Line") :- ()
    AdjacentStops("Wentworthville","Pendle Hill","T3 Bankstown Line") :- ()
    AdjacentStops("Pendle Hill","Toongabbie","T3 Bankstown Line") :- ()
    AdjacentStops("Toongabbie","Seven Hills","T3 Bankstown Line") :- ()
    AdjacentStops("Seven Hills","Blacktown","T3 Bankstown Line") :- ()
    AdjacentStops("Doonside","Rooty Hill","T3 Bankstown Line") :- ()
    AdjacentStops("Rooty Hill","Mount Druitt","T3 Bankstown Line") :- ()
    AdjacentStops("Mount Druitt","St Marys","T3 Bankstown Line") :- ()
    AdjacentStops("St Marys","Werrington","T3 Bankstown Line") :- ()
    AdjacentStops("Werrington","Kingswood","T3 Bankstown Line") :- ()
    AdjacentStops("Kingswood","Penrith","T3 Bankstown Line") :- ()
    AdjacentStops("Penrith","Emu Plains","T3 Bankstown Line") :- ()
    AdjacentStops("Marayong","Quakers Hill","T3 Bankstown Line") :- ()
    AdjacentStops("Quakers Hill","Schofields","T3 Bankstown Line") :- ()
    AdjacentStops("Schofields","Riverstone","T3 Bankstown Line") :- ()
    AdjacentStops("Riverstone","Vineyard","T3 Bankstown Line") :- ()
    AdjacentStops("Vineyard","Mulgrave","T3 Bankstown Line") :- ()
    AdjacentStops("Mulgrave","Windsor","T3 Bankstown Line") :- ()
    AdjacentStops("Windsor","Clarendon","T3 Bankstown Line") :- ()
    AdjacentStops("Clarendon","East Richmond","T3 Bankstown Line") :- ()
    AdjacentStops("East Richmond","Richmond","T3 Bankstown Line") :- ()
    AdjacentStops("Bondi Junction","Edgecliff","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Edgecliff","Kings Cross","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Kings Cross","Martin Place","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Martin Place","Town Hall","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Town Hall","Central","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Central","Redfern","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Redfern","Sydenham","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Sydenham","Tempe","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Tempe","Wolli Creek","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Wolli Creek","Arncliffe","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Arncliffe","Banksia","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Banksia","Rockdale","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Rockdale","Kogarah","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Kogarah","Carlton","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Carlton","Allawah","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Allawah","Hurstville","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Hurstville","Penshurst","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Penshurst","Mortdale","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Mortdale","Oatley","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Oatley","Como","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Como","Jannali","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Jannali","Sutherland","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Loftus","Engadine","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Engadine","Heathcote","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Heathcote","Waterfall","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Kirrawee","Gymea","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Gymea","Miranda","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Miranda","Caringbah","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Caringbah","Woolooware","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Woolooware","Cronulla","T4 Eastern Suburbs & Illawarra Line") :- ()
    AdjacentStops("Schofields","Quakers Hill","T5 Cumberland Line") :- ()
    AdjacentStops("Quakers Hill","Marayong","T5 Cumberland Line") :- ()
    AdjacentStops("Marayong","Blacktown","T5 Cumberland Line") :- ()
    AdjacentStops("Blacktown","Seven Hills","T5 Cumberland Line") :- ()
    AdjacentStops("Seven Hills","Toongabbie","T5 Cumberland Line") :- ()
    AdjacentStops("Toongabbie","Pendle Hill","T5 Cumberland Line") :- ()
    AdjacentStops("Pendle Hill","Wentworthville","T5 Cumberland Line") :- ()
    AdjacentStops("Wentworthville","Westmead","T5 Cumberland Line") :- ()
    AdjacentStops("Westmead","Parramatta","T5 Cumberland Line") :- ()
    AdjacentStops("Parramatta","Harris Park","T5 Cumberland Line") :- ()
    AdjacentStops("Harris Park","Merrylands","T5 Cumberland Line") :- ()
    AdjacentStops("Merrylands","Guildford","T5 Cumberland Line") :- ()
    AdjacentStops("Guildford","Yennora","T5 Cumberland Line") :- ()
    AdjacentStops("Yennora","Fairfield","T5 Cumberland Line") :- ()
    AdjacentStops("Fairfield","Canley Vale","T5 Cumberland Line") :- ()
    AdjacentStops("Canley Vale","Cabramatta","T5 Cumberland Line") :- ()
    AdjacentStops("Cabramatta","Warwick Farm","T5 Cumberland Line") :- ()
    AdjacentStops("Warwick Farm","Liverpool","T5 Cumberland Line") :- ()
    AdjacentStops("Liverpool","Casula","T5 Cumberland Line") :- ()
    AdjacentStops("Casula","Glenfield","T5 Cumberland Line") :- ()
    AdjacentStops("Glenfield","Macquarie Fields","T5 Cumberland Line") :- ()
    AdjacentStops("Macquarie Fields","Ingleburn","T5 Cumberland Line") :- ()
    AdjacentStops("Ingleburn","Minto","T5 Cumberland Line") :- ()
    AdjacentStops("Minto","Leumeah","T5 Cumberland Line") :- ()
    AdjacentStops("Leumeah","Campbelltown","T5 Cumberland Line") :- ()
    AdjacentStops("Schofields","Clyde","T6 Carlingford Line") :- ()
    AdjacentStops("Clyde","Rosehill","T6 Carlingford Line") :- ()
    AdjacentStops("Rosehill","Camellia","T6 Carlingford Line") :- ()
    AdjacentStops("Camellia","Rydalmere","T6 Carlingford Line") :- ()
    AdjacentStops("Rydalmere","Dundas","T6 Carlingford Line") :- ()
    AdjacentStops("Dundas","Telopea","T6 Carlingford Line") :- ()
    AdjacentStops("Telopea","Carlingford","T6 Carlingford Line") :- ()
    AdjacentStops("Town Hall","Wynyard","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Wynyard","Circular Quay","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Circular Quay","St James","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("St James","Museum","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Museum","Central","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Green Square","Mascot","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Mascot","Domestic Airport","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Domestic Airport","International Airport","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("International Airport","Wolli Creek","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Turrella","Bardwell Park","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Bardwell Park","Bexley North","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Bexley North","Kingsgrove","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Kingsgrove","Beverly Hills","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Beverly Hills","Narwee","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Narwee","Riverwood","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Riverwood","Padstow","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Padstow","Revesby","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Revesby","Panania","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Panania","East Hills","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("East Hills","Holsworthy","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Glenfield","Macquarie Fields","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Macquarie Fields","Ingleburn","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Ingleburn","Minto","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Minto","Leumeah","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Leumeah","Campbelltown","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Campbelltown","Macarthur","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Museum","St James","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("St James","Circular Quay","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Circular Quay","Wynyard","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Wynyard","Town Hall","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Town Hall","Central","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Redfern","Macdonaldtown","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Macdonaldtown","Newtown","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Newtown","Stanmore","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Stanmore","Petersham","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Petersham","Lewisham","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Lewisham","Summer Hill","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Summer Hill","Ashfield","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Ashfield","Croydon","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Croydon","Burwood","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Burwood","Strathfield","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Strathfield","Homebush","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Homebush","Flemington","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Flemington","Lidcombe","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Lidcombe","Auburn","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Auburn","Granville","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Merrylands","Guildford","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Guildford","Yennora","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Yennora","Fairfield","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Fairfield","Canley Vale","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Canley Vale","Cabramatta","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Cabramatta","Warwick Farm","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Warwick Farm","Liverpool","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Liverpool","Casula","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Casula","Glenfield","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Glenfield","Macquarie Fields","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Macquarie Fields","Ingleburn","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Ingleburn","Minto","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Minto","Leumeah","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Leumeah","Campbelltown","T2 Airport, Inner West & South Line") :- ()
    AdjacentStops("Edmondson Park","Leppington","T2 Airport, Inner West & South Line") :- ()

    val a, b, c = program.variable()
    Connected(a, b) :- AdjacentStops(a,b,__)
    Connected(a, b) :- ( Connected(a, c), AdjacentStops(c,b,__) )
    
    ReachableStops("Central", b) :- Connected("Central", b)
  }
}
