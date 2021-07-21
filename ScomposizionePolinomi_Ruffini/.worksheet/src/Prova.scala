import Math._
  object Prova {
  
    //classe Polinomio
    class Polinomio(c: List[Int]){
      var coeff = c
    
      def ValorePolinomio(x:Int):Int = {
        var value:Int = 0
        var i = 0
        for(c <- this.coeff){
          value = value + c*pow(x,i).toInt
          i = i+1
        }
        value
       }
    }
    
    //classi figlie: Parabola, Retta
    class Parabola(v: Int, w:Int, u:Int) extends Polinomio(List(v,w,u)){
      var a = u
      var b = w
      var c = v
      
      def ValoreParabola(x:Int):Int = {
        var value = this.a*pow(x,2).toInt + this.b*x + this.c
        value
      }
    }
  
    class Retta(v:Int, w:Int) extends Polinomio(List(v,w)){
      var q = v
      var m = w
      
      def ValoreRetta(x:Int):Int = {
        var value = this.m*x + this.q
        value
      }
    };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(1503); 

    //Funzione per trovare i divisori
    def RicercaDivisori(p:Polinomio):List[Int] = {

      //se il polinomio è una retta o un numero, non ci sono divisori
      if(p.coeff.size <= 2)
        return List.empty;
     
      val termineNoto = p.coeff(0)
      val coeffGradoMax = p.coeff(p.coeff.size-1)
      val div = termineNoto/coeffGradoMax
      
      if(div < 1 && div > -1)
        return List.empty;
      
      var divisori:List[Int] = List.empty
      if (div > 0)
        divisori = List.range(-div-1, div+1)
      else
        divisori = List.range(div-1, -div+1)
      divisori.filter(x => x!=0 && div.%(x) == 0 && p.ValorePolinomio(x) == 0)
    };System.out.println("""RicercaDivisori: (p: Prova.Polinomio)List[Int]""");$skip(1179); 
    
    //Regola di Bernoulli
    def ScomposizioneBernoulli(p:Polinomio, r:Int = 0):Unit = {
      var div:List[Int] = RicercaDivisori(p);
      
      //se non ci sono ne divisori ne radici, polinomio non scomponibile
      if(div.size == 0 && r == 0){
        println(">> Polinomio non scomponibile.");
        return;
      }
      
      //se non ci sono più divisori, ma c'è almeno una radice
      //stampo i coefficienti rimanenti
      if(div.size == 0 && r > 0){
        println("Coefficienti: " + p.coeff.toString());
        println(">> Scomposizione terminata.");
        return;
      }
      
      //se c'è almeno un divisore
      //stampo la radice scelta e faccio una nuova iterazione
      if(div.size > 0){
        var radice = div(0)
        var newCoeff:List[Int] = List()
        var index = 0
        for(i <- p.coeff){
          if(index == 0) newCoeff = newCoeff.::(i)
          else if(index == p.coeff.size-1){}
          else newCoeff = newCoeff.::(newCoeff(index-1)*radice+i)
          index = index+1
        }
        println("Radice " + (r+1) + ": " + radice);
        return ScomposizioneBernoulli(new Polinomio(newCoeff), r+1);
      }
    };System.out.println("""ScomposizioneBernoulli: (p: Prova.Polinomio, r: Int)Unit""")}
    
    
    

}
