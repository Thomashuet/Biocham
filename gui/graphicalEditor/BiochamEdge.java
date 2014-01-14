package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JLabel;


public class BiochamEdge extends DefaultEdge{

	
	
	int stoich;
	
	 public BiochamEdge(){
	     this(null);
	 }
	 
	
	 
	 public BiochamEdge(Object userObject){		
	
		 super.setUserObject(userObject);
		 BiochamEdgeData userObj=(BiochamEdgeData)userObject;
		 
		 if(userObj.getStoichiometry()>1){
			 stoich=userObj.getStoichiometry();
					
				if(userObj.getStoichiometry()>1){
					BiochamGraphConstants.setLabelAlongEdge(this.getAttributes(), true);
					BiochamGraphConstants.setLabelEnabled(this.getAttributes(),true);
					BiochamGraphConstants.setExtraLabels(this.getAttributes(),new Object[]{new JLabel(Integer.toString(userObj.getStoichiometry()))});
					BiochamGraphConstants.setRouting(this.getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
					BiochamGraphConstants.setLineStyle(this.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
					BiochamGraphConstants.setBendable(this.getAttributes(), true);
					BiochamGraphConstants.setSelectable(this.getAttributes(),false);
					
				}else{
					/*BiochamGraphConstants.setLineStyle(this.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
					BiochamGraphConstants.setLineStyle(super.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);*/
					BiochamGraphConstants.setRouting(this.getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
					BiochamGraphConstants.setLineStyle(this.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
					BiochamGraphConstants.setBendable(this.getAttributes(), true);
				}
		 }
			BiochamGraphConstants.setRouting(this.getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
			BiochamGraphConstants.setLineStyle(this.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
			BiochamGraphConstants.setBendable(this.getAttributes(), true);
			/* BiochamGraphConstants.setLineStyle(super.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
		 BiochamGraphConstants.setLineStyle(this.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
		 BiochamGraphConstants.setBendable(this.getAttributes(), true);*/
		
		 userObj=null;
	 }

	 public BiochamEdge(int stoichoimetry){
		 
		super(null);
		BiochamGraphConstants.setRouting(this.getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
		BiochamGraphConstants.setLineStyle(this.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
		BiochamGraphConstants.setBendable(this.getAttributes(), true);
		
	 }
	     
	 public void setSource(Object src){
		 super.setSource(src);
	 }
	 public void setTarget(Object target){
		 super.setTarget(target);
	 }
	
	 public String toString(){
		 if(stoich>1){
			 return Integer.toString(stoich);	 
		 }else{
			 return "";
		 }
		 
	 }

	
	 
	
}
