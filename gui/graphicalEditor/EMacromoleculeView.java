package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellViewRenderer;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.VertexRenderer;
import org.jgraph.graph.VertexView;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.StringTokenizer;

import javax.swing.BorderFactory;





public class EMacromoleculeView extends VertexView{

	
	public transient MacromoleculeRenderer renderer=new MacromoleculeRenderer();	
	protected BiochamEntityData userObject;
	Rectangle2D bounds;
	public static Rectangle handle = new Rectangle(0, 0, 7, 7);
	
	public EMacromoleculeView(Object cell) {
		super(cell);	
		
		if(cell instanceof EMacromoleculeCell){
			double x,y;
			x=0;
			y=0;
			userObject=((BiochamEntityData)((EMacromoleculeCell)cell).getUserObject());	
			if(userObject.getPosition()!=null){
				//System.out.println(userObject.getPosition().getX()+","+userObject.getPosition().getY());
				if(!(userObject.getPosition().getX()>0 && userObject.getPosition().getY()>0)){
					bounds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
					if(bounds!=null){
						x=bounds.getX();
						y=bounds.getY();		
					}		
					userObject.setPosition(new Position(x,y,0,0));					
				}
			}else{
				bounds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
				if(bounds!=null){
					x=bounds.getX();
					y=bounds.getY();		
				}		
				userObject.setPosition(new Position(x,y,0,0));	
			}
			
			double w=EMacromoleculeCell.WIDTH;
			double h=EMacromoleculeCell.HEIGHT;
			if(userObject.getSize()==null){
				if(bounds!=null){
					w=bounds.getWidth();
					h=bounds.getHeight();
				}
				userObject.setSize(new MoleculeSize(w,h));
			}
			userObject.setBounds(bounds);
			
		}	
		userObject.getGraph().refresh();
	}
	
	
	public CellViewRenderer getRenderer() {
		return renderer;
	}
	
	public static int getArcSize(int width, int height) {
         int arcSize;
         if (width <= height) {
             arcSize = height / 5;
             if (arcSize > (width / 2))
                 arcSize = width / 2;
         } else {
             arcSize = width / 5;
             if (arcSize > (height / 2))
                 arcSize = height / 2;
         }
         return arcSize;
     }

	
	public class MacromoleculeRenderer extends VertexRenderer {
		
		protected boolean isRichText = false, stretchImage = false, isEditing = false, showFoldingIcons = true, isGroup = false;
		

		 protected void paintFoldingIcon(Graphics g) {
			                 if (isGroup) {
			                    g.setColor(Color.white);
			                     g.fill3DRect(handle.x, handle.y, handle.width,
			                             handle.height, true);
			                     g.setColor(Color.white);
			                     g.drawRect(handle.x, handle.y, handle.width, handle.height);
			                     int h2 = handle.y + handle.height / 2;
			                     g.drawLine(handle.x + 1, h2, handle.x + handle.width - 2,
			                             h2);
			                     if (view.isLeaf()) {
			                         int w2 = handle.x + handle.width / 2;
			                         g.drawLine(w2, handle.y + 1, w2, handle.y
			                                 + handle.height - 2);
			                     }
			                 }
			             }

		
		public Dimension getPreferredSize() {
			Dimension d = super.getPreferredSize();
			d.width += d.width/8 ;
			d.height+=d.height/2;
			return d;
		}
		
		public void paint(Graphics g) {
			
			
			setDoubleBuffered(true);
			setOpaque(false);
			
			int b = borderWidth;
			Graphics2D g2 = (Graphics2D) g;			
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);
			g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
					RenderingHints.VALUE_TEXT_ANTIALIAS_ON);	
			Dimension d = getSize();			
			int roundRectArc = getArcSize(d.width - b, d.height - b);	
			boolean tmp = selected;
		
			/*if(userObject!=null){
				if(userObject.getCompartment()!=null){
					DefaultGraphCell comp=GraphUtilities.getCellByName(userObject.getGraph(),userObject.getCompartment());
					if(comp!=null){
						if((comp.getUserObject())!=null && comp.getUserObject() instanceof BiochamCompartmentData){
							if(((BiochamCompartmentData)comp.getUserObject()).getBounds()!=null){
								double x=((BiochamCompartmentData)comp.getUserObject()).getBounds().getX();
								double y=((BiochamCompartmentData)comp.getUserObject()).getBounds().getY();
								double x1=x+((BiochamCompartmentData)comp.getUserObject()).getBounds().getWidth();
								double y1=y+((BiochamCompartmentData)comp.getUserObject()).getBounds().getHeight();
								if(bounds!=null){
									System.out.println("\n\n\n\n\nMOLECULE before: ("+bounds.getX()+","+bounds.getY()+"), AFTER: from ("+x+","+y+"),to ("+x1+","+y1+").\n\n\n\n\n\n");	
								}else{
									userObject.setBounds(g.getClipBounds());
									bounds=g.getClipBounds();
									System.out.println("\n\n\n\n\nMOLECULE before: ("+bounds.getX()+","+bounds.getY()+"), AFTER: from ("+x+","+y+"),to ("+x1+","+y1+").\n\n\n\n\n\n");
								}
							}
						}
					}
					
					
					
				}
			}*/
			
			g.setColor(super.getBackground());			
			int x=20, y=15, z=15;
			if(d.width<80){
				x=10;
				y=10;
				z=5;
				
			}
			if(userObject.getColor()==null){
				if(userObject.isModulator()){
					gradientColor=Color.RED.darker();
				}else{				
					gradientColor=Color.GREEN.darker();
				}
			}else{
				gradientColor=userObject.getColor();
			}			
			if (gradientColor != null && !preview) {
				
				setOpaque(false);				
				int card=userObject.getMultimerCardinality();
				if(card>1){		
				   // System.out.println("INT="+d.width+","+d.height);
					g.setClip(0,0,d.width+10,d.height+10);
				
					if(userObject.isCopy()){
						
						Rectangle rect=g2.getClipBounds();
						//System.out.println("(x,y)_0="+rect.getX()+","+rect.getY());
						g2.setPaint(Color.BLACK);
						g.drawRoundRect(x, y, d.width- z, d.height- (int) (5*b * 1.5), roundRectArc, roundRectArc);	
						
						g2.setColor(Color.GRAY);
						g2.fillRoundRect(x, y, d.width- z, d.height- (int) (5*b * 1.5),roundRectArc,roundRectArc);	
						
						g2.setPaint(new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true));
						g2.setClip(x, y, d.width- z, (d.height- (int) (5*b * 1.5))-(d.height- (int) (5*b * 1.5))/4);						
						//the background..........						
						g.fillRoundRect(x,y, d.width- z, d.height- (int) (5*b * 1.5), roundRectArc, roundRectArc);						
												
						//System.out.println("(x,y)_1="+rect.getX()+","+rect.getY());
						rect=g2.getClipBounds();
						//System.out.println("(x,y)_2="+rect.getX()+","+rect.getY());
						g2.setClip(0, 0, d.width, d.height);
						rect=g2.getClipBounds();
						//System.out.println("(x,y)_3="+rect.getX()+","+rect.getY());
						//the foreground........
						int t=d.height;
						g2.setColor(Color.gray);
						g2.fillRoundRect(0, 0, d.width- (int) (b * 1.5), t,roundRectArc,roundRectArc);	
					
						g2.setPaint(new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true));
						g2.setClip(0, 0, d.width- (int) (b * 1.5), t-t/3);		
						g.fillRoundRect(0, 0, d.width- (int) (b * 1.5), t, roundRectArc, roundRectArc);						
						
						g2.setPaint(Color.WHITE);			
						g2.setClip(0, 0, d.width, d.height);
						
						Font f0=g.getFont();
						int inc=2*d.height/10+2;
						Font f1=new Font(f0.getFamily(),Font.BOLD,inc);
						g.setFont(f1);
										
						if(userObject.getCopyInstance()>=10){
							g.drawString(""+userObject.getCopyInstance(),d.width/2-8,13*d.height/14 );
						}else{
							g.drawString(""+userObject.getCopyInstance(),d.width/2-4,13*d.height/14 );
						}	
						f1=null;
						g.setFont(f0);
						g2.setPaint(Color.GRAY);
					}else{
						int w=d.width;
						int h=d.height;						
						Paint p=new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true);						
						drawMultimerMacromolecule(g, b, g2, roundRectArc, x, y, z, w, h, p);												
						g2.setPaint(Color.GRAY);
						p=null;
					}
							
					g.setClip(-20,-20,d.width+20,d.height+20);
			
					String cardinality="N:"+Integer.toString(userObject.getMultimerCardinality());
					if(userObject.getMultimerCardinality()/10<1){
						g.drawRect(d.width/13,-16,24,15);						
					}else{
						g.drawRect(d.width/13,-16,31,15);
					}
					g.drawString(cardinality,d.width/13+2,-4);
					
					if(userObject.getGraph().isShowInitialConcentration()){					
						g.drawString(userObject.getInitialConcentration(),d.width/2,-1);
					}
					
					try {
						setBorder(null);
						setOpaque(false);
						selected = false;
						super.paint(g);
					} finally {
						selected = tmp;
					}
					bordercolor=Color.black;;
					setBorder(BorderFactory.createRaisedBevelBorder());
					if (bordercolor != null) {
						g.setColor(bordercolor);
						g2.setStroke(new BasicStroke(b));
						g.drawRoundRect(b / 2, b / 2,d.width - (int) (b * 1.5) - 1, d.height- (int) (b * 1.5), roundRectArc, roundRectArc);                                
					}
				}else{
					
					if(userObject.isCopy()){
						g2.setColor(Color.gray);
						g2.fillRoundRect(0, 0, d.width- (int) (b * 1.5), d.height,roundRectArc,roundRectArc);	
					
						g2.setPaint(new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true));
						g2.setClip(0, 0, d.width- (int) (b * 1.5), d.height-(d.height/3)+d.height/15-1);		
						g.fillRoundRect(0, 0, d.width- (int) (b * 1.5), d.height, roundRectArc, roundRectArc);						
						
						g2.setPaint(Color.WHITE);			
						g2.setClip(0, 0, d.width, d.height);
						Font f0=g.getFont();
						int inc=2*d.height/10+2;
						Font f1=new Font(f0.getFamily(),Font.BOLD,inc);
						g.setFont(f1);
						if(userObject.getCopyInstance()>=10){
							g.drawString(""+userObject.getCopyInstance(),d.width/2-8,14*d.height/15 );
						}else{
							g.drawString(""+userObject.getCopyInstance(),d.width/2-4,14*d.height/15 );
						}	
						f1=null;
						g.setFont(f0);
						g2.setPaint(Color.GRAY);
						
					}else{
						g2.setPaint(new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true));
						g.fillRoundRect(0, 0, d.width- (int) (b * 1.5), d.height, roundRectArc, roundRectArc);
					}
					
					g.setClip(-20,-20,d.width+20,d.height+20);
					g2.setPaint(Color.GRAY);
					if(userObject.getGraph().isShowInitialConcentration()){
						g.drawString(userObject.getInitialConcentration(),d.width/2,-1);
					}
					try {
						setBorder(null);
						setOpaque(false);
						selected = false;
						super.paint(g);
					} finally {
						selected = tmp;
					}
					bordercolor=Color.black;;
					setBorder(BorderFactory.createRaisedBevelBorder());
					if (bordercolor != null) {
						g.setColor(bordercolor);
						g2.setStroke(new BasicStroke(b));
						g.drawRoundRect(b / 2, b / 2,d.width - (int) (b * 1.5) - 1, d.height- (int) (b * 1.5), roundRectArc, roundRectArc);                                
					}
				}										
			}				
			if(userObject.getMoleculeState()==MoleculeState.MODIFIED){
				g.setClip(-5,-5,d.width+340,d.height+40);
				g2.setPaint(Color.black);
				g2.setStroke(new BasicStroke(1));
				Font f=g.getFont();
				BiochamGraph.setCellsFont(f);
				g.drawOval(d.width- (int) (b * 1.5), -2,16,16);
				g.drawString("m",d.width- (int) (b * 1.5)+3,10);
				if(userObject.getModificationSites()!=null){
					FontRenderContext frc = new FontRenderContext(null, false, false);	
					int len=(int)g.getFont().getStringBounds(userObject.getModificationSites(), frc).getWidth();
					int min_0=len;
					//if((userObject.getNumberOfModifSites()*userObject.getModificationSites().length()/2>0)){
						//min_0=userObject.getModificationSites().length()*5+d.width/(userObject.getNumberOfModifSites()*userObject.getModificationSites().length()/2);//(10-userObject.getNumberOfModifSites())+d.width/10;
					//}					
					//min_0+=(userObject.getModificationSites().length()/userObject.getNumberOfModifSites())*userObject.getModificationSites().length()/userObject.getNumberOfModifSites()/2+userObject.getNumberOfModifSites()*(userObject.getNumberOfModifSites()/3-userObject.getModificationSites().length()/3+2);
					//min_0+=7*((min_0/userObject.getNumberOfModifSites())/40);
					/*if(userObject.getNumberOfModifSites()==1){
						if(userObject.getModificationSites().length()<2){
							min_0-=d.width-len;
						}else{
							min_0-=len;
						}
					}*/
					if(userObject.getModificationSites().length()<2){
						min_0+=2;
					}else if(len>=d.width-20){
						min_0-=36*(len-d.width)/37+userObject.getNumberOfModifSites()*2+5;
						
					}
					//System.out.println("\nmacromolecule="+userObject.getName()+",lenSites="+len+",min_0="+min_0+",d.width="+d.width);
					/*if(userObject.getNumberOfModifSites()==1){
						if(d.width-min_0>=1){
							System.out.println("\nmacromolecule="+userObject.getName()+",d.width-min_0="+(d.width-min_0));
							min_0+=d.width/4;
							//min_0-=userObject.getModificationSites().length();
						}else if(userObject.getModificationSites().length()<3){
							min_0+=d.width/4;
						}
						System.out.println("\nAFTER:macromolecule="+userObject.getName()+",lenSites="+userObject.getModificationSites().length()+",min_0="+min_0);
					}else if(userObject.getNumberOfModifSites()>3){
						if(userObject.getModificationSites().length()/userObject.getNumberOfModifSites()>=3){
							min_0+=3*userObject.getModificationSites().length()/4;
						}
					}*/
					//System.out.println("\n\n+adding="+d.width/(userObject.getNumberOfModifSites()*40)+"sites="+userObject.getNumberOfModifSites()+", length="+userObject.getModificationSites().length()+"**MACROMOLECULE**len="+min_0+"\n****d.width="+d.width);
					/*if(min_0>150){
						min_0+=8;
					}*/
					/*if(d.width-min_0<=20){
						min_0+=2;
					}
					min_0+=userObject.getModificationSites().length()/userObject.getNumberOfModifSites();*/
					/*if(userObject.getModificationSites().length()/userObject.getNumberOfModifSites()>=3){
						if(min_0+2*userObject.getModificationSites().length()-d.width<=20){
							min_0+=userObject.getModificationSites().length()/2+userObject.getModificationSites().length()/userObject.getNumberOfModifSites();
						}
						
					}*/
					if(len>=d.width-10){
						Utils.debugMsg("\n\n*************************before="+userObject.getSize().width);
						if(Math.abs(userObject.getSize().width-(d.width+(len-d.width)/4))>5){
							userObject.getSize().width=d.width+(len-d.width)/4;		
						//	min_0+=17;
							userObject.applySize();		
						}	
						
									
					}
					Font f0=g.getFont();
					Font f1=new Font(f0.getFamily(),Font.PLAIN,10);
					g.setFont(f1);	
					g2.setPaint(Color.black);
					//g2.setPaint(new Color(115,115,115));//96,123,139));//131,139,131));//0,128,128));//85,85,85));
					g.drawString(userObject.getModificationSites(),d.width-min_0,12);
					f1=null;
					g.setFont(f0);
				}				
			}		
			g2.setPaint(Color.black);
			if(userObject.getRepresentingName()==null){
				boolean set=false;
				if(userObject.getMoleculeState()==MoleculeState.MODIFIED){
					if(userObject.getName().contains("~")){
						userObject.setRepresentingName(userObject.getName().substring(0,userObject.getName().indexOf("~")));
						set=true;
					}					
				}
				if(!set){
					userObject.setRepresentingName(userObject.getName());
				}
			}
			if(userObject.getRepresentingName()!=null){
				
				int increment=0;
				if(userObject.getMoleculeState()==MoleculeState.MODIFIED){
					increment=5;						
				}
				if(userObject.getRepresentingName().contains("-")){
					StringTokenizer stTemp=new StringTokenizer(userObject.getRepresentingName(),"-");
					String tokenBefore=stTemp.nextToken();
					int counter=1;
					String name="";
					while(stTemp.hasMoreTokens()){
						String token=stTemp.nextToken();
						if(token.equals(tokenBefore)){
							counter++;
						}else{
							if(counter>1){						
								name+=tokenBefore;
							}
							if(name!=""){
								name+="-"+tokenBefore;
							}else{
								name+=tokenBefore;
							}
							counter=1;
							tokenBefore=token;
						}
					}	
					if(counter>1){	
						name+=tokenBefore;						
					}
					if(userObject.getSize().getWidth()/name.length()<11){
						//System.out.println("\n\n**beforeeee....MACROMOLECULE**nameLenght="+name.length()+"\n****width="+userObject.getSize().getWidth());
						userObject.getSize().width+=11*name.length()+3;
						userObject.applySize();
						//System.out.println("\n\n**afterrrr.....MACROMOLECULE**nameLenght="+name.length()+"\n****width="+userObject.getSize().getWidth());
					}
					
					
					if(userObject.isCopy()){
						g.drawString(name,d.width/2-g.getFontMetrics().stringWidth(name)/2,d.height/3+g.getFontMetrics().getHeight()/3 +increment);
					
					}else{
						g.drawString(name,d.width/2-g.getFontMetrics().stringWidth(name)/2,d.height/2+g.getFontMetrics().getHeight()/2 +increment);
					}
				}else{
					if(userObject.getSize().getWidth()/userObject.getRepresentingName().length()<11){
						userObject.getSize().width=11*userObject.getRepresentingName().length()+3;
						userObject.applySize();
					}
					//System.out.println("2****"+userObject.getRepresentingName().length());					
					if(userObject.isCopy()){						
						g.drawString(userObject.getRepresentingName(),d.width/2-g.getFontMetrics().stringWidth(userObject.getRepresentingName())/2,d.height/3+g.getFontMetrics().getHeight()/3 +increment);				
						
					}else{
						g.drawString(userObject.getRepresentingName(),d.width/2-g.getFontMetrics().stringWidth(userObject.getRepresentingName())/2,d.height/2+g.getFontMetrics().getHeight()/2  +increment);
					}
					
					
				}
			}
			
			//double pX=userObject.getPosition().getX();
			//double pY=userObject.getPosition().getY();
			
			//if(){}
			Position pos=userObject.getPosition();
			
			if(pos!=null){
				
				Rectangle2D bnds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
				if(bnds!=null){
					
					double xc=pos.getX();
					double yc=pos.getY();
					double xx=bnds.getX();
					double yy=bnds.getY();
					EMacromoleculeCell iv=(EMacromoleculeCell)cell;
					iv.setX(xx);
					iv.setY(yy);
					pos.setX(xx);
					pos.setY(yy);
				}
			}			
		}

		/**
		 * @param g
		 * @param b
		 * @param g2
		 * @param roundRectArc
		 * @param x
		 * @param y
		 * @param z
		 * @param w
		 * @param h
		 * @param p
		 */
		private void drawMultimerMacromolecule(Graphics g, int b, Graphics2D g2, int roundRectArc, int x, int y, int z, int w, int h, Paint p) {
			g2.setPaint(Color.black);
			g.drawRoundRect(x, y, w- z, h- (int) (5*b * 1.5), roundRectArc, roundRectArc);		
			g2.setPaint(p);
			g.fillRoundRect(x,y, w- z, h- (int) (5*b * 1.5), roundRectArc, roundRectArc);			
			g2.setPaint(p);
			g.fillRoundRect(0, 0, w- (int) (b * 1.5), h, roundRectArc, roundRectArc);
		}
		
		public Point2D getPerimeterPoint(VertexView view, Point2D source, Point2D p) {
			Rectangle2D bounds = view.getBounds();
			double x = bounds.getX();
			double y = bounds.getY();
			double width = bounds.getWidth();
			double height = bounds.getHeight();
			double xCenter = x + width / 2;
			double yCenter = y + height / 2;
			double dx = p.getX() - xCenter; // Compute Angle
			double dy = p.getY() - yCenter;
			double alpha = Math.atan2(dy, dx);
			double xout = 0, yout = 0;
			double pi = Math.PI;
			double pi2 = Math.PI / 2.0;
			double beta = pi2 - alpha;
			double t = Math.atan2(height, width);
			if (alpha < -pi + t || alpha > pi - t) { // Left edge
				xout = x;
				yout = yCenter - width * Math.tan(alpha) / 2;
			} else if (alpha < -t) { // Top Edge
				yout = y;
				xout = xCenter - height * Math.tan(beta) / 2;
			} else if (alpha < t) { // Right Edge
				xout = x + width;
				yout = yCenter + width * Math.tan(alpha) / 2;
			} else { // Bottom Edge
				yout = y + height;
				xout = xCenter + height * Math.tan(beta) / 2;
			}
			return new Point2D.Double(xout, yout);
		}		
	}
	public String toString(){
		return "";//cell.toString();
	}
}
