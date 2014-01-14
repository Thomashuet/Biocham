package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Map;

public class IntermediateVertexDissoc extends DefaultGraphCell implements MiddleVertex{

	
	public static int WIDTH=10;
	public static int HEIGHT=10;	
	public double X=180;
	public double Y=100;
	Object userObject;
	BiochamGraph graph;
	/** Reference to the bounds attribute */
	protected Rectangle2D bounds;
	public static int counter=0;
	Color color;
	public double newX, newY;
	
	public IntermediateVertexDissoc(BiochamGraph graph,Object cell,double x, double y) {		
		
		super(cell);
		
		this.userObject=cell;		
		this.graph=graph;
		if(x!=0 && y!=0){
			BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(x,y,WIDTH,HEIGHT));
			setX(x);
			setY(y);
		}	
		initialize();		 
	}
	
	public IntermediateVertexDissoc(BiochamGraph graph) {
		
		 super();
		 initialize();
		 this.graph=graph;
	}
	
	
	public IntermediateVertexDissoc(BiochamGraph graph,Object cell){
		
		super(cell);		
		this.userObject=cell;
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
	
	/*public IntermediateVertexDissoc(IntermediateVertex center,Object uo) {
		
		userObject=uo;
		boolean set=false;
		BiochamEdgeData dt=null;
		if(uo instanceof BiochamEdgeData){
			dt=(BiochamEdgeData)uo;
			if(dt.getPosition()!=null){
				Position pos=dt.getPosition();
				if(pos!=null){				
					if(pos.getX1()!=null && pos.getY1()!=null){
						if(pos.getX1()>0 && pos.getY1()>0){
							 BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(pos.getX1(),pos.getY1(),WIDTH,HEIGHT));
							 set=true;
						}
					}					
				}
			}
		}
		if(!set){
			X=center.getX()+50;
			Y=center.getY()+2;
			BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(X,Y,WIDTH,HEIGHT));
			if(dt!=null){
				dt.setPosition(new Position(X,Y,0,0));
			}			
		}
		
		 BiochamGraphConstants.setEditable(this.getAttributes(), false);
		 BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		 BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		 BiochamGraphConstants.setSizeable(this.getAttributes(), false);		
		 BiochamGraphConstants.setResize(this.getAttributes(), false);			
		 BiochamGraphConstants.setConstrained(this.getAttributes(),true);
		 BiochamGraphConstants.setDisconnectable(this.getAttributes(), false);
		 
	 }*/
	
	
	private void initialize() {	
		BiochamGraphConstants.setEditable(this.getAttributes(), true);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSizeable(this.getAttributes(), false);		
		BiochamGraphConstants.setResize(this.getAttributes(), false);			
		BiochamGraphConstants.setConstrained(this.getAttributes(),true);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(), false);		 
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
		
		
		if(userObject instanceof BiochamEdgeData){
			
			BiochamEdgeData dt=(BiochamEdgeData)userObject;					
			if(dt.getPosition()!=null){				
				
				dt.getPosition().setX(x);
				dt.getPosition().setY(y);
				
			}else{
				
				dt.setPosition(new Position(x,y,0,0));					
			}	
			
			dt=null;
		}
	}
	

	public double getX() {
		return X;
	}

	public void setX(double x) {
		if(userObject instanceof BiochamEdgeData){
			BiochamEdgeData dt=(BiochamEdgeData)userObject;
			if(dt.getPosition()!=null){
		
				dt.getPosition().setX1(x);
			}
		}
		X = x;
	}

	public double getY() {
		return Y;
	}

	public void setY(double y) {
		if(userObject instanceof BiochamEdgeData){
			BiochamEdgeData dt=(BiochamEdgeData)userObject;
			if(dt.getPosition()!=null){
				
				dt.getPosition().setY1(y);
			}
		}
		Y = y;
	}
	    
	public Object getUserObject() {
		return userObject;
	}

	public void setUserObject(Object userObject) {
		this.userObject = userObject;
	}
	 
	public Position getPosition() {
		if(userObject instanceof BiochamEdgeData){
			if(((BiochamEdgeData)userObject).getPosition()!=null){
				return ((BiochamEdgeData)userObject).getPosition();
			}else{
				((BiochamEdgeData)userObject).setPosition(new Position(X,Y,0,0));				
				return ((BiochamEdgeData)userObject).getPosition();
			}
		}
		return null;
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
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBorderColor(attributeMap1, color);
		BiochamGraphConstants.setLineColor(attributeMap1, color);
    	BiochamGraphConstants.setBendable(attributeMap1,false);
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested, null, null, null);
		}
		if(this.userObject instanceof BiochamEdgeData){
			
			((BiochamEdgeData)this.userObject).setColor(color);
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
