package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellViewRenderer;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.VertexRenderer;
import org.jgraph.graph.VertexView;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;






public class ECompartmentView extends VertexView{

	public CompartmentRenderer renderer=new CompartmentRenderer();
	protected BiochamCompartmentData userObject;
	public static int callings1=0,callings2=0;
	
	
	public ECompartmentView(Object cell) {
		super(cell);		
		if(cell instanceof ECompartmentCell){
			
			double x,y;
			x=0;
			y=0;
			userObject=((BiochamCompartmentData)((ECompartmentCell)cell).getUserObject());		
			bounds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
			if(bounds!=null){
				x=bounds.getX();
				y=bounds.getY();
				
			}		
			userObject.setPosition(new Position(x,y,0,0));		
				
		}		
		//System.out.println("\n***********CALLING COMPART_VIEW: "+callings1);
		callings1++;
		userObject.setBounds(renderer.getBounds());
	}
	
		
	public CellViewRenderer getRenderer() {
		return renderer;
	}
	
	public String toString(){
		return cell.toString();
	}
	
	private int getArcSize(int width, int height) {
        int arcSize;
        if (width <= height) {
            arcSize = height / 2;
            if (arcSize > (width))
                arcSize = width;
        } else {
            arcSize = width / 2;
            if (arcSize > (height))
                arcSize = height;
        }
        return arcSize;
    }
	
	public class CompartmentRenderer extends VertexRenderer {
		
		
		public Dimension getPreferredSize() {
			Dimension d = super.getPreferredSize();
			d.width += d.width / 8;
			d.height += d.height / 2;
			return d;
		}
		
		
		public void paint(Graphics g) {
			
			//System.out.println("\n***********CALLING COMPART_PAINT: "+callings2);
			callings2++;
			int b = borderWidth;
			Graphics2D g2 = (Graphics2D) g;
			Dimension d = getSize();
			setDoubleBuffered(true);
			setOpaque(true);
		//System.out.println("*****************COMPARTMENT BEFORE: width="+d.width+",height="+d.height);
			userObject.setBounds(g.getClipBounds());
			
			int roundRectArc = getArcSize(d.width - b, d.height - b);
			boolean tmp = selected;
			g.setColor(super.getBackground());
			if(userObject!=null){
				if(userObject.getColor()!=null){
					gradientColor=userObject.getColor();
				}else{
					/******************PROBLEM HERE IN GENERATING RANDOM COLORS************/
					int index=userObject.getGraph().getNumberOfCompartments()-(userObject.getGraph().getNumberOfCompartments()/Utils.compartmentColors.length);
					if(!(index>Utils.compartmentColors.length-1 || index<0)){
						gradientColor=Utils.compartmentColors[index];	
					}else{
						userObject.getGraph().setNumberOfCompartments(0);
						gradientColor=Utils.compartmentColors[0];
					}
					
					//gradientColor=Utils.gradientYellow;
				}
			}else{
				gradientColor=Utils.compartmentColors[userObject.getGraph().getNumberOfCompartments()-(userObject.getGraph().getNumberOfCompartments()/Utils.compartmentColors.length)];
				int index=userObject.getGraph().getNumberOfCompartments()-(userObject.getGraph().getNumberOfCompartments()/Utils.compartmentColors.length);
				if(!(index>Utils.compartmentColors.length-1 || index<0)){
					gradientColor=Utils.compartmentColors[index];	
				}else{
					userObject.getGraph().setNumberOfCompartments(0);
					gradientColor=Utils.compartmentColors[0];
				}
				//gradientColor=Utils.gradientYellow;
			}
			//new Color(255, 250, 205);//yellow255,236,139);//217,217,243);//255,225,255);//lightPink:255,228,225);//veryLightGreen:240,255,240);//255,228,196);//sandColor:245,245,220);//);//Color.GREEN.brighter();
			userObject.setColor(gradientColor);
			if (gradientColor != null && !preview) {
				
				//setOpaque(false);
				g.setClip(-20,-20,d.width+20,d.height+20);
				g2.setStroke(new BasicStroke(10));
				g2.setPaint(new GradientPaint(0, 0, gradientColor.brighter(),0, getHeight(), gradientColor, false));
				//g.fillRoundRect(0, 2, d.width-5, d.height- (int) (3*b * 1.5), roundRectArc/2, roundRectArc/2);
				//g2.setPaint(new GradientPaint(0, 0, gradientColor,0, getHeight(), gradientColor.darker(), false));//gradientColor);
				g2.setPaint(gradientColor);
				g.drawRoundRect(0, 2, d.width-5, d.height- (int) (3*b * 1.5), roundRectArc/2, roundRectArc/2);
			}
						
			try {
				//setBorder(null);
				setOpaque(false);
				selected = false;
				super.paint(g);
			} finally {
				selected = tmp;
			}
			
			//gradientColor=Color.gray;
			//g2.setPaint(new GradientPaint(0, 0, gradientColor.brighter(),0, getHeight(), gradientColor.darker(), false));
			//String s=;
			//int len=;
			//g.setClip(0,-20,d.width+20,d.height+20);
			//System.out.println("w="+d.width+",h="+d.height);			
			//g.drawRect(d.width/2-50,-15,g.getFontMetrics().stringWidth(userObject.getVolume())+8,20);	
			//g.drawString(userObject.getVolume(),d.width/2-45,-1);	
			g2.setPaint(Color.black);
			if(userObject.getCompartmentName()==null){
				userObject.setCompartmentName("");
			}
			g.drawString(userObject.getCompartmentName(),d.width/2-g.getFontMetrics().stringWidth(userObject.getCompartmentName())/2,d.height-g.getFontMetrics().getHeight() );			
		}
	}
}

