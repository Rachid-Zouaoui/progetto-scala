import Math._

object Main {
  
    trait Printable {
      def print: Unit
    }
    
    //classe Polinomio
    class Polinomio(c: List[Int]) extends Printable{
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
      
      override def print = println("Polinomio con coefficienti (da grado minore a grado maggiore): " + this.coeff.toString())
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
      
      override def print = println("Parabola: a=" + this.a + ", b=" + this.b + ", c=" + this.c)
    }
  
    class Retta(v:Int, w:Int) extends Polinomio(List(v,w)){
      var q = v
      var m = w
      
      def ValoreRetta(x:Int):Int = {
        var value = this.m*x + this.q
        value
      }
      
      override def print = println("Retta: m=" + this.m + ", q=" + this.q)
    }

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
    }
    
    
    //Scomposizione di un polinommio con Ruffini
    def ScomposizioneRuffini(p:Polinomio)(r:Int = 0):Unit = {
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
        return ScomposizioneRuffini(new Polinomio(newCoeff))(r+1);
      }      
    }
    
    //-----------HIGH ORDER FUNCTION-------------
    //funzione d'appoggio per valutare una parabola
    def ValParabola(p:Polinomio, x:Int):Int = {
      p.coeff(2)*pow(x,2).toInt + p.coeff(1)*x + p.coeff(0)
    }
    //funzione d'appoggio per valutare un polinomio
    def ValPolinomio(p:Polinomio, x:Int):Int = {
      var value:Int = 0
      var i = 0
      for(c <- p.coeff){
        value = value + c*pow(x,i).toInt
        i = i+1
      }
      value
    }
    
    //funzione differente: passiamo una funzione di valutazione come input!
    def RicercaDivisoriHOF(p:Polinomio)(f:(Polinomio, Int) => Int):List[Int] = {
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
      divisori.filter(x => x!=0 && div.%(x) == 0 && f(p, x) == 0)      
    }
    
    def ScomposizioneRuffiniHOF(p:Polinomio)(r:Int = 0):Unit = {
      var div:List[Int] = List()
      if(p.coeff.size == 3)
        div = RicercaDivisoriHOF(p)(ValParabola);
      else
        div = RicercaDivisoriHOF(p)(ValPolinomio);
      
      if(div.size == 0 && r == 0){
        println(">> Polinomio non scomponibile.");
        return;
      }
      
      if(div.size == 0 && r > 0){
        println("Coefficienti: " + p.coeff.toString());
        println(">> Scomposizione terminata.");
        return;
      }
      
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
        return ScomposizioneRuffini(new Polinomio(newCoeff))(r+1);
      }      
    }
    
    //Main
    def main(args: Array[String]) : Unit = {
      
      //definizione Polinomi per effettuare alcune prove:
      var polinomio = new Polinomio(List(-24,-2,5,1))
      var retta = new Retta(4,3)
      var parabola1 = new Parabola(-1,1,3)
      var parabola2 = new Parabola(-10,3,1)
      
      //cerco le radici di tutti i polinomi creati (senza HOF)
      polinomio.print
      ScomposizioneRuffini(polinomio)(0)
      println("")
      parabola1.print
      ScomposizioneRuffini(parabola1)(0)
      println("")
      retta.print
      ScomposizioneRuffini(retta)(0)
      println("")
      parabola2.print
      ScomposizioneRuffini(parabola2)(0)
      println("")
      
      //ora faccio la stessa cosa ma sfruttando HOF
      polinomio.print
      ScomposizioneRuffiniHOF(polinomio)(0)
      println("")
      parabola1.print
      ScomposizioneRuffiniHOF(parabola1)(0)
      println("")
    }
}