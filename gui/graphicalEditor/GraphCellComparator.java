package fr.inria.contraintes.biocham.graphicalEditor;

import java.util.Comparator;
import org.jgraph.JGraph;
import org.jgraph.graph.*;
import com.jgraph.layout.JGraphFacade;

/** 
 * Compare two graph verticies; we can compare based on different
 * properties depending on the compareMetric value.  Default is to
 * compare based on number of neighbors
 * @author Bobby Krupczak, rdk@krupczak.org
 * @version $Id: GraphCellComparator.java 42 2008-08-04 02:09:23Z rdk $
 **/

public class GraphCellComparator implements Comparator {

   /* class variables and methods *********************** */
   public static int COMPARE_EDGES = 1;
   public static int COMPARE_NEIGHBORS = 2;

   /* instance variables ******************************** */
   public int compareMetric;
   public JGraphFacade theFacade;

   /* constructors  ************************************* */
   GraphCellComparator() 
   { 
       compareMetric = COMPARE_NEIGHBORS;
       return; 
   }

   public GraphCellComparator(JGraphFacade f, int compareMetric) 
   { 
       this.compareMetric = compareMetric;
       this.theFacade = f;

       if ((this.compareMetric < COMPARE_EDGES) || 
	   (this.compareMetric > COMPARE_NEIGHBORS))
           this.compareMetric = COMPARE_NEIGHBORS;

       return; 
   }

   /* private methods *********************************** */

   /* public methods ************************************ */

   public int getCompareMetric() { return compareMetric; }
   public void setCompareMetric(int compareMetric)
   {
       this.compareMetric = compareMetric;

       if ((this.compareMetric < COMPARE_EDGES) || 
	   (this.compareMetric > COMPARE_NEIGHBORS))
           this.compareMetric = COMPARE_NEIGHBORS;

       return; 
   }

   public int compare(Object a, Object b)
   {
       DefaultGraphCell a1, b1;

       // dig out the verticies       
       a1 = (DefaultGraphCell)a;
       b1 = (DefaultGraphCell)b;

       if (compareMetric == COMPARE_EDGES) {
	   return theFacade.getEdges(b1).length - 
	          theFacade.getEdges(a1).length;
       }
       else {
	  // compare number of neighbors
	  return theFacade.getNeighbours(b1,false).size() -
	         theFacade.getNeighbours(a1,false).size();
       }
   }

} /* class GraphCellComparator */
