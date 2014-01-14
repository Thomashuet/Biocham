package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.JGraph;
import org.jgraph.graph.DefaultCellViewFactory;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.EdgeView;
import org.jgraph.graph.GraphLayoutCache;
import org.jgraph.graph.GraphModel;
import org.jgraph.graph.PortView;
import org.jgraph.graph.VertexView;

import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Transparency;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import javax.imageio.ImageIO;
import javax.swing.JInternalFrame;


public final class BiochamGraph extends JGraph{
	

	
	
	public BiochamGraph biochamGraph;
	public static Font font;
	public static JInternalFrame graphEditorFrame;
	public Dimension graphSize;
	public HashMap<String,Object[]> containingRules;
	public BiochamModel biochamModel;
	private boolean showInitialConcentration=true;
	private boolean showReactionKinetics=true;
	public static String[] states=new String[]{MoleculeState.MODIFIED, MoleculeState.NONE};
	public ArrayList<GraphReactions> reactionsModifications;
	BiochamGraphEditorDesktop graphEditorInstance;
	PopupMenuBiochamReaction reactionPopup;
	PopupMenuBiochamEntity moleculePopup;
	PopupMenuBiochamCompartment compartmentPopup;
	DefaultGraphCell middleEdgeInstance;
	PopupMenuBiochamGraph graphPopup;
	boolean allAdded=false;
	boolean ruleDeleted=true;
	BiochamGraphListener listener;
	boolean compactGraph=false;
	public static int numberOfCompartments=0;
	/*Constructor*/
	public BiochamGraph(BiochamModel currentModel,JInternalFrame parent){
	
		super();
		
		
		
		biochamModel=currentModel;
		Utils.debugMsg("\n\n\n\nBiochamGraph: Set CurrentModel="+currentModel.getModelName()+"\n\n\n\n");
		graphEditorFrame=parent;
		biochamGraph=this;		
		graphSize=parent.getSize();
		font=getCellsFont();
		listener=new BiochamGraphListener(biochamGraph);
		reactionsModifications=new ArrayList<GraphReactions>();
		containingRules=new HashMap<String,Object[]>();
		moleculePopup=new PopupMenuBiochamEntity(this,listener);
		reactionPopup=new PopupMenuBiochamReaction(this,listener);
		compartmentPopup=new PopupMenuBiochamCompartment(this,listener);
		graphPopup=new PopupMenuBiochamGraph(this);
		setBiochamGraphUI();	
		System.setProperty("sun.java2d.d3d", "false");
		this.setSize(graphSize);
		this.setAutoResizeGraph(true);
		this.setBendable(false);
		this.setEdgeLabelsMovable(true);
		this.setDoubleBuffered(true);
		this.setToolTipText(currentModel.getModelName());		
		this.setOpaque(false);// gives better performance.....
				
	}
	private void setBiochamGraphUI() {
		
		
		GraphModel model = new BiochamGraphModel(this);
		setModel(model);
		super.setModel(model);
	
		GraphLayoutCache view = new GraphLayoutCache(model,	new	DefaultCellViewFactory());
		this.setGraphLayoutCache(view);
		this.setModel(model);
		setCloneable(true);		
		
		getGraphLayoutCache().setFactory(new DefaultCellViewFactory() { 
		    
		      protected VertexView createVertexView(Object cell) { 
		    	 
		    	  if (cell instanceof EMacromoleculeCell) {		    		
		    		  return new EMacromoleculeView(cell);
		    	  }else if(cell instanceof ESourceSink){
		    		  return new ESourceSinkNode(cell);
		    	  }else if(cell instanceof ENucleicAcidFeatureCell){			    		
		    		  return new ENucleicAcidFeatureView(cell);
		    	  }else if(cell instanceof EComplexCell){		    		  
		    		  return new EComplexView(cell);
		    	  }else if(cell instanceof ECompartmentCell){
		    		  return new ECompartmentView(cell);
		    	  }else if(cell instanceof IntermediateVertex){
		    		  return new IntermediateVertexNode(cell);
		    	  }else if(cell instanceof IntermediateVertexAssoc){
		    		  return new IntermediateVertexAssocNode(cell);
		    	  }else if(cell instanceof IntermediateVertexDissoc){
		    		  return new IntermediateVertexDissocNode(cell);		    		  
		    	  }else if(cell instanceof IntermediateVertex2){
		    		  return new IntermediateVertexNode2(cell);		    		  
		    	  }else if(cell instanceof IntermediateVertex3){
		    		  return new IntermediateVertexNode3(cell);		    		  
		    	  }
		    	  return new VertexView(cell); 
		      }
		
		      @Override
		      protected EdgeView createEdgeView(Object cell){
		    	   
		    	  if(cell instanceof BiochamEdge){
		    		  
		    		  return new BiochamEdgeView(cell);
		    	  }else{
		    		  PortView.allowPortMagic=true;
		    	  }
		    	  return new BiochamEdgeView(cell);
		      }
		      
		      protected  PortView createPortView(Object port ) {
		    	    
		    	    if (port instanceof DefaultPort) {
		    	       return  new BiochamPortView(port);
		    	    }
		    	    
		    	    return new PortView(port);
		      }

		 }); 
		
		setEditable(false);
		setJumpToDefaultPort(true);		
		setAntiAliased(false);
		setBendable(false);
		setDoubleBuffered(false);	
		setOpaque(true);		
		getGraphLayoutCache().setHidesDanglingConnections(true);		
		this.setEdgeLabelsMovable(true);		
		this.setJumpToDefaultPort(true);
		this.setEdgeLabelsMovable(true);
		this.setMoveIntoGroups(false);
		addMouseListener(new BiochamGraphListener(this));
	

		model=null;
		view=null;
	}
		
	
	/*Graph Methods*/	
	public BiochamGraph getBiochamGraph() {
		
		return biochamModel.getReactionsGraphEditor().getBiochamGraph();
	}
	public void setBiochamGraph(BiochamGraph bg) {
		biochamGraph = bg;
	}	
	public static void setCellsFont(Font f){
		font=f;
	}	
	public static Font getCellsFont(){
		return font;
	}
	public HashMap<String,Object[]> getContainingRules() {
		return containingRules;
	}
	public void setContainingRules(HashMap<String,Object[]> containingRules) {
		this.containingRules = containingRules;
	}
	public BiochamModel getBiochamModel() {
		return biochamModel;
	}
	public void setBiochamModel(BiochamModel biochamModel) {
		this.biochamModel = biochamModel;
	}
	public boolean isAllAdded() {
		return allAdded;
	}
	public void setAllAdded(boolean allAdded) {
		this.allAdded = allAdded;
	}
	public boolean isRuleDeleted() {
		return ruleDeleted;
	}
	public void setRuleDeleted(boolean ruleDeleted) {
		this.ruleDeleted = ruleDeleted;
	}
	public void exportAsImage(final String nm) {
		
		
		 SwingWorker sw=new SwingWorker(){
			
			@Override
			public Object construct() {
				try {
					String name=nm;
					if(!name.contains(".")){
						name+=".png";
					
					}
				
					if(!name.endsWith(".*")){
						name+=".png";
						
					}else if(name.indexOf(".")<0){
						name+=".png";
						
					}
					BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(name));
					BufferedImage image = getImage(getBackground(),50);
					ImageIO.write(image, "png", out);
					out.close();
					image=null;
					name=null;
					out=null;
				} catch (Exception e) {
					e.printStackTrace();
					
				}
				return null;
			}

			@Override
			public void finished() {
				// TODO Auto-generated method stub
				
			}};
		 sw.start();
			 
	     
		//}
			
}
	public ArrayList<GraphReactions> getReactionsModifications() {
		return reactionsModifications;
	}
	public void setReactionsModifications(ArrayList<GraphReactions> reactionsModifications) {
		this.reactionsModifications = reactionsModifications;
	}
	
	public boolean isShowInitialConcentration() {
		return showInitialConcentration;
	}
	public void setShowInitialConcentration(boolean showInitialConcentration) {
		this.showInitialConcentration = showInitialConcentration;
	}
	public boolean isShowReactionKinetics() {
		return showReactionKinetics;
	}
	public void setShowReactionKinetics(boolean showReactionKinetics) {
		this.showReactionKinetics = showReactionKinetics;
	}
		
	
	/*Other Methods*/	
	public void updateReaction(String rule, String newRule) {
	
		if(newRule==null){
			reactionsModifications.add(new GraphReactions(rule,newRule));	
		}else{
			boolean exists=false;
			for(int i=0;i<reactionsModifications.size();i++){				
				if(reactionsModifications.get(i).getModifiedRule()!=null){
					if(reactionsModifications.get(i).getOriginalRule().equals(rule) || reactionsModifications.get(i).getModifiedRule().equals(rule)){					
						reactionsModifications.get(i).setModifiedRule(newRule);
						exists=true;
						break;
					}
				}else{
					if(reactionsModifications.get(i).getOriginalRule().equals(rule)){
						reactionsModifications.get(i).setModifiedRule(newRule);
						exists=true;
						break;
					}
				}
				
			}
			if(!exists){			
				reactionsModifications.add(new GraphReactions(rule,newRule));	
			}
		}
	}
	public void deleteReaction(String toDelete,boolean fromEverywhere) {
		
		String toAdd = null;
		if(containingRules.get(toDelete)==null){
			DefaultGraphCell[] allR=GraphUtilities.getAllReactions(this);			
			for(int i=0;i<allR.length;i++){
				BiochamEdgeData dt=GraphUtilities.getBiochamEdgeDataFromCell(allR[i]);
				if(dt!=null){
					if(dt.getName()!=null){
						//System.out.println(dt.getName());
						if(dt.getName().contains(toDelete) && !dt.getName().equals(toDelete)){
							
							
							String tmp=dt.getName().substring(1,dt.getName().length()-1);
							String tmp1=tmp.substring(0,tmp.indexOf(","));
							String tmp2=tmp.substring(tmp.indexOf(",")+1);
							if(tmp1.equals(toDelete)){
								toAdd=tmp2;
							}else{
								toAdd=tmp1;
							}
							toDelete=dt.getName();
							tmp=null;
							tmp1=null;
							tmp2=null;						
							break;
						}
					}
				}
				dt=null;
			}
			allR=null;
			
		}
		
		if(containingRules.get(toDelete)!=null){	
				
			ArrayList w=new ArrayList();		
			Object[] obj=containingRules.get(toDelete);
			Utils.debugMsg(obj.length);
			
			for(int i=0;i<obj.length;i++){
				
				if(obj[i]!=null){
					Utils.debugMsg(obj[i].getClass().toString());
				}
					
				
					if(containingRules.get(toDelete)[i]!=null){
						Utils.debugMsg(containingRules.get(toDelete)[i].getClass().toString());
						boolean isOther=containingRules.get(toDelete)[i] instanceof ReversibleMiddleEdge || containingRules.get(toDelete)[i] instanceof MiddleEdge ||
						containingRules.get(toDelete)[i] instanceof IntermediateVertex || containingRules.get(toDelete)[i] instanceof IntermediateVertex2 || 
						containingRules.get(toDelete)[i] instanceof IntermediateVertex3 || containingRules.get(toDelete)[i] instanceof IntermediateVertexAssoc || 
						containingRules.get(toDelete)[i] instanceof IntermediateVertexDissoc || containingRules.get(toDelete)[i] instanceof DefaultEdge || containingRules.get(toDelete)[i] instanceof ESourceSink;
						
						if(GraphUtilities.isBiochamEntity(containingRules.get(toDelete)[i]) && !(isOther)){
							
							DefaultGraphCell c=(DefaultGraphCell)containingRules.get(toDelete)[i];
							BiochamEntityData data=(BiochamEntityData)c.getUserObject();
							Utils.debugMsg(data.getName()+",isModulator="+data.isModulator());
							int connections=data.getNumberOfConnections();							
							if(connections<2){
								w.add(containingRules.get(toDelete)[i]);					
							}else{								
								data.setNumberOfConnections(connections-1);
								if(data.getInvolvedInReactions().contains(toDelete)){
									data.getInvolvedInReactions().remove(toDelete);
								}
								c.setUserObject(data);
							}
							c=null;
							data=null;
							connections=0;
						}else if(isOther){
							w.add(containingRules.get(toDelete)[i]);
						}else{
							w.add(containingRules.get(toDelete)[i]);
						}
					}
			}
			
			this.getModel().remove(this.getDescendants(w.toArray()));			
			containingRules.remove(toDelete);	
			if(fromEverywhere){
				reactionsModifications.remove(toDelete);
			}
			w.clear();
			w=null;
			obj=null;
			
		}else{
			if(this.getSelectionCells()!=null){
				if(this.getSelectionCells().length>0){
					this.getModel().remove(this.getSelectionCells());
				}
			}
			
		}
		if(toAdd!=null){
			this.getBiochamModel().parserBcRule2Graph.parse(toAdd);	
		}
		toAdd=null;	
	}

	
	public DefaultGraphCell getMiddleEdgeInstance() {		
		return middleEdgeInstance;
	}

	public void setMiddleEdgeInstance(DefaultGraphCell mei) {
		middleEdgeInstance=mei;
	}

	@Override
	public BufferedImage getImage(Color bg, int inset) {
		// TODO, this method could just use the offscreen if available
		Object[] cells = getRoots();
		Rectangle2D bounds1 = getCellBounds(cells);
		Rectangle2D bounds=new Rectangle2D.Double(bounds1.getX(),bounds1.getY(),bounds1.getWidth()+20,bounds1.getHeight()+20);
		if (bounds != null) {
			toScreen(bounds);
			GraphicsConfiguration graphicsConfig = getGraphicsConfiguration();
			BufferedImage img = null;
			if (graphicsConfig != null) {
				img = getGraphicsConfiguration().createCompatibleImage(
						(int) bounds.getWidth() + 2 * inset,
						(int) bounds.getHeight() + 2 * inset,
						(bg != null) ? Transparency.OPAQUE
								: Transparency.BITMASK);
			} else {
				img = new BufferedImage((int) bounds.getWidth() + 2 * inset,
						(int) bounds.getHeight() + 2 * inset,
						(bg != null) ? BufferedImage.TYPE_INT_RGB
								: BufferedImage.TYPE_INT_ARGB);
			}

			Graphics2D graphics = img.createGraphics();
			if (bg != null) {
				graphics.setColor(bg);
				graphics.fillRect(0, 0, img.getWidth(), img.getHeight());
			} else {
				graphics.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f));
				graphics.fillRect(0, 0, img.getWidth(), img.getHeight());
				graphics.setComposite(AlphaComposite.SrcOver);
			}
			graphics.translate((int) (-bounds.getX() + inset), (int) (-bounds.getY() + inset));
			print(graphics);
			graphics.dispose();
			cells=null;
			bounds1=null;
			bounds=null;
			graphicsConfig=null;
			graphics=null;
			return img;
		}
		return null;
	}
	public boolean isCompactGraph() {
		return compactGraph;
	}
	public void setCompactGraph(boolean compactGraph) {
		this.compactGraph = compactGraph;
	}
	public static int getNumberOfCompartments() {
		return numberOfCompartments;
	}
	public static void setNumberOfCompartments(int numberOfCompartments) {
		BiochamGraph.numberOfCompartments = numberOfCompartments;
	}
	public BiochamGraphEditorDesktop getGraphEditorInstance() {
		return graphEditorInstance;
	}
	public void setGraphEditorInstance(BiochamGraphEditorDesktop graphEditorInstance) {
		this.graphEditorInstance = graphEditorInstance;
	}
	
	
	
	

}	
	