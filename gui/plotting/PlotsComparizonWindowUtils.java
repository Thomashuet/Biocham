package fr.inria.contraintes.biocham.plotting;

import org.jdesktop.swingx.MultiSplitLayout;
import org.jdesktop.swingx.MultiSplitPane;
import org.jdesktop.swingx.MultiSplitLayout.Node;
import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.customComponents.DroppableMultiSplitPane;
import fr.inria.contraintes.biocham.customComponents.PlotIconPanel;
import fr.inria.contraintes.biocham.customComponents.ScrollingPanel;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.BorderLayout;
import java.util.ArrayList;
import javax.swing.ImageIcon;
import javax.swing.JPanel;



public class PlotsComparizonWindowUtils {

	
	
	 public static void updateComparisonWindow(){
     	
		 	BiochamDynamicTree.comparizonWindow.removeAll();	    
		    SwingWorker sw=new SwingWorker(){

		    	MultiSplitLayout.Node modelRoot;
				@Override
				public Object construct() {
					String layoutDef =  defineNewLayout(); 
					modelRoot = MultiSplitLayout.parseModel(layoutDef);
					MultiSplitPane msp=buildMultiSplitPane(modelRoot);
					msp.setBackground(Utils.backgroundColor);
					BiochamDynamicTree.comparizonWindow.add(msp,BorderLayout.CENTER);	
					BiochamDynamicTree.comparizonWindow.repaint();
					layoutDef=null;
					msp=null;
					return null;
				}
				@Override
				public void finished() {				
			        modelRoot=null;
			        BiochamDynamicTree.comparizonWindow.revalidate();  
				}};
		    sw.start();       	      
	    }
	    
	    private static String defineNewLayout() {
	    			
			int nPlots=BiochamDynamicTree.jplots.size();			
			if(nPlots%2>0){
				nPlots++;
			}
			ArrayList<String> list=new ArrayList<String>();		
			StringBuilder buf=new StringBuilder();
			String child1,child2,rowi;
			for(int i=0;i<nPlots/2;i++){
				child1="left"+(i+1);
				child2="right"+(i+1);
				rowi="(ROW "+child1 +" "+child2+")";
				list.add(rowi);
			}
			child1=null;
			child2=null;
			rowi=null;	
			buf.append("(COLUMN (ROW left0 right0)");
			for(int i=0;i<list.size();i++){		
				buf.append(" ");
				buf.append(list.get(i));
			}		
			buf.append(")");
			list.clear();
			list=null;
			return buf.toString();		
		}
	   
	   
	    private static DroppableMultiSplitPane buildMultiSplitPane(Node modelRoot) {
			
	    	DroppableMultiSplitPane multiSplitPane=new DroppableMultiSplitPane();
			multiSplitPane.setDividerSize(5);
	        multiSplitPane.getMultiSplitLayout().setModel(modelRoot);        
	        int nPlots=BiochamDynamicTree.jplots.size();        
	        ImageIcon jp1,jp2;    
	        String t;
	        JPanel pan;
	        ScrollingPanel d;
	        
	        if(nPlots%2==0){
	        	for(int i=0;i<nPlots/2;i++){
	        		
	        		jp1=BiochamDynamicTree.jplots.get(2*i);        		       		
	        		pan=new PlotIconPanel(jp1);
	     	        t="left"+(i);
	     	        d=new ScrollingPanel(pan);
	     	     	multiSplitPane.add(d,t);   	     	
	     	     	//multiSplitPane.getDragAndDropPanels().add(d);	     	     	
	        		jp2=BiochamDynamicTree.jplots.get(2*i+1);
	        		pan=new PlotIconPanel(jp2);
	     	        t="right"+(i);
	     	        d=new ScrollingPanel(pan);
	    	     	multiSplitPane.add(d,t);   	     	
	    	     	//multiSplitPane.getDragAndDropPanels().add(d);  	     	
	    	     		     	
	        	}      	
	        	
	        }else{
	        	
	        	int i=0;
	        	for(i=0;i<nPlots/2;i++){
	        		
	        		jp1=BiochamDynamicTree.jplots.get(2*i);        		        		
	        		pan=new PlotIconPanel(jp1);
	     	        t="left"+(i);
	     	        d=new ScrollingPanel(pan);
	    	     	multiSplitPane.add(d,t);   	     	
	    	     //	multiSplitPane.getDragAndDropPanels().add(d);	     	       	       	
	        		jp2=BiochamDynamicTree.jplots.get(2*i+1);
	        		pan=new PlotIconPanel(jp2);
	     	        t="right"+(i);
	     	        d=new ScrollingPanel(pan);
	   	     		multiSplitPane.add(d,t);   	     	
	   	     		//multiSplitPane.getDragAndDropPanels().add(d);	
	        				
	        	}
	        	jp1=BiochamDynamicTree.jplots.get(2*i);
	        	pan=new PlotIconPanel(jp1); 	
	 	        if(nPlots==1){
	 	        	t="left1";
	 	        }else{
	 	        	t="left"+(i);
	 	        }
	 	        d=new ScrollingPanel(pan);
		     	multiSplitPane.add(d,t);   	     	
		     	//multiSplitPane.getDragAndDropPanels().add(d);	
		     	
	 	      
	        } 
	        t=null;
	        jp1=null; jp2=null;      
	        pan=null; d=null;        
	        return multiSplitPane;     
		}
	    
}
