package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Map;






public class IntermediateVertex extends DefaultGraphCell implements MiddleVertex{

	public static int WIDTH=12;
	public static int HEIGHT=12;
	public double X=250;
	public double Y=100;
	public Rectangle2D defaultBounds = new Rectangle2D.Double(250,100,WIDTH,HEIGHT);
	BiochamGraph graph;
	Object edgeUserObject;
	Color color;
	public static int counter=0;
	public double newX, newY;
	protected Rectangle2D bounds;

	/*public void refresh(){
		if(this.edgeUserObject instanceof BiochamEdgeData){
			((IntermediateVertexNode)((BiochamEdgeData)this.edgeUserObject).drawingInstance).getRenderer().=null;
		}
	}*/
	public IntermediateVertex(BiochamGraph graph,Object cell,double x, double y) {		
		
		super(cell);
		
		this.edgeUserObject=cell;		
		this.graph=graph;
		if(x!=0 && y!=0){
			BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(x,y,WIDTH,HEIGHT));
			setX(x);
			setY(y);
		}	
		initialize();		 
	}
	
	public IntermediateVertex(BiochamGraph graph) {
		
		 super();
		 initialize();
		 this.graph=graph;
	}
	
	
	public IntermediateVertex(BiochamGraph graph,Object cell){
		
		super(cell);		
		this.edgeUserObject=cell;
		this.graph=graph;
		
		if(cell instanceof BiochamEdgeData){
			
			
			BiochamEdgeData d=(BiochamEdgeData)cell;
			Position pos=d.getPosition();
			
			if(pos!=null){
				
				try{
					
					double x,y;
					x=pos.getX();
					y=pos.getY();
					
					BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(x,y,WIDTH,HEIGHT));
					setX(x);
					setY(y);
					
					if(cell instanceof BiochamEdgeData){
					
						BiochamEdgeData dt=(BiochamEdgeData)cell;						
						if(dt.getPosition()!=null){
							dt.getPosition().setX(x);
							dt.getPosition().setY(y);
						}else{
							dt.setPosition(new Position(x,y,0,0));
						}						
						dt=null;
					}
				}catch(Exception e){
					e.printStackTrace();
				}
				
			}else{
			
				Location l=null;
				if(d.getGraph().getBiochamModel().isLoadingModel()){
					l=GraphUtilities.getDownFreeLocation(graph);
					if(l.getX()>0 && l.getY()>150){
						X=l.getX();
						Y=l.getY();
					}
				}else{
					if(d.getGraph().getBiochamModel().getCounter()==3){
						l=GraphUtilities.getFreeYLocation(graph);
						d.getGraph().getBiochamModel().setCounter(1);
						if(l.getX()>0 && l.getY()>150){
							X=l.getX();
							Y=l.getY();					
						}
					}else{
						l=GraphUtilities.getFreeXLocation(graph);
						int cnt=d.getGraph().getBiochamModel().getCounter();
						cnt+=1;
						d.getGraph().getBiochamModel().setCounter(cnt);
						if(l!=null){
							if(l.getY()>0 && l.getX()>150){
								X=l.getX();
								Y=l.getY();					
							}
						}else{
							Utils.debugMsg("l is null");
						}						
					}
				}
				
				
				BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(X,Y,WIDTH,HEIGHT));
				setX(X);
				setY(Y);
				
				if(cell instanceof BiochamEdgeData){
					
					BiochamEdgeData dt=(BiochamEdgeData)cell;					
					if(dt.getPosition()!=null){				
						
						dt.getPosition().setX(X);
						dt.getPosition().setY(Y);
						
					}else{
						
						dt.setPosition(new Position(X,Y,0,0));					
					}	
					
					dt=null;
				}
			}
			d=null;pos=null;
		}		
		initialize();
	}	
	
	
	private void initialize() {	
		BiochamGraphConstants.setEditable(this.getAttributes(), false);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSizeable(this.getAttributes(), false);		
		BiochamGraphConstants.setResize(this.getAttributes(), false);			
		BiochamGraphConstants.setConstrained(this.getAttributes(),true);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(), false);		 
	}  
	
	
	public double getX(){
		if(this.edgeUserObject instanceof BiochamEdgeData){
			BiochamEdgeData dt=(BiochamEdgeData)this.edgeUserObject;
			if(dt.getPosition()!=null){			
				return dt.getPosition().getX();
			}else{				
				return X;				
			}			
		}
		return X;
	}
	
	public double getY(){
		if(this.edgeUserObject instanceof BiochamEdgeData){
			BiochamEdgeData dt=(BiochamEdgeData)this.edgeUserObject;
			if(dt.getPosition()!=null){			
				return dt.getPosition().getY();
			}else{				
				return Y;				
			}			
		}
		return Y;
	}

	public void setX(double x) {
		if(this.edgeUserObject instanceof BiochamEdgeData){
			BiochamEdgeData dt=(BiochamEdgeData)this.edgeUserObject;
			if(dt.getPosition()!=null){		
				dt.getPosition().setX(x);
			}else{
				dt.setPosition(new Position(x,0,0,0));				
			}
			dt=null;
		}
		X = x;
	}

	public void setY(double y) {
		if(this.edgeUserObject instanceof BiochamEdgeData){
			BiochamEdgeData dt=(BiochamEdgeData)this.edgeUserObject;
			if(dt.getPosition()!=null){		
				dt.getPosition().setY(y);
			}else{
				dt.setPosition(new Position(0,y,0,0));				
			}
			
			dt=null;
		}
		Y = y;
	}

	public Position getPosition(){
		if(edgeUserObject instanceof BiochamEdgeData){
			if(((BiochamEdgeData)edgeUserObject).getPosition()!=null){
				return ((BiochamEdgeData)edgeUserObject).getPosition();
			}else{
				((BiochamEdgeData)edgeUserObject).setPosition(new Position(X,Y,0,0));				
				return ((BiochamEdgeData)edgeUserObject).getPosition();
			}
		}
		return null;
	}
	
	public void setBounds(double x, double y){
		
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
				
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,WIDTH,HEIGHT));
		setX(x);
		setY(y);
		
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}	
		
		
		if(edgeUserObject instanceof BiochamEdgeData){
			
			BiochamEdgeData dt=(BiochamEdgeData)edgeUserObject;					
			if(dt.getPosition()!=null){				
				
				dt.getPosition().setX(x);
				dt.getPosition().setY(y);
				
			}else{
				
				dt.setPosition(new Position(x,y,0,0));					
			}	
			
			dt=null;
		}
	}
	
	public BiochamGraph getGraph() {
		return graph;
	}

	public void setGraph(BiochamGraph graph) {
		this.graph = graph;
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		
		this.color = color;
		
		if(this.edgeUserObject instanceof BiochamEdgeData){
			
			((BiochamEdgeData)this.edgeUserObject).setColor(color);
			BiochamGraphConstants.setBorderColor(this.getAttributes(), color);
		}
		
	}
	public void setNewY(double y2) {
		newY=y2;		
	}
	public void setNewX(double x2) {
		newX=x2;		
	}
	
}
