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
import java.awt.image.BufferedImage;
import java.util.StringTokenizer;

import javax.swing.BorderFactory;



public class ENucleicAcidFeatureView extends VertexView{

	
	public transient NucleicAcidFeatureRenderer renderer=new NucleicAcidFeatureRenderer();	
	protected BiochamEntityData userObject;
	Rectangle2D bounds;
	
	public ENucleicAcidFeatureView(Object cell) {
		super(cell);
		
		if(cell instanceof ENucleicAcidFeatureCell){
			double x,y;
			x=0;
			y=0;
			userObject=((BiochamEntityData)((ENucleicAcidFeatureCell)cell).getUserObject());		
			bounds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
			if(bounds!=null){
				x=bounds.getX();
				y=bounds.getY();		
			}		
			userObject.setPosition(new Position(x,y,0,0));
			double w=ENucleicAcidFeatureCell.WIDTH;
			double h=ENucleicAcidFeatureCell.HEIGHT;
			if(bounds!=null){
				w=bounds.getWidth();
				h=bounds.getHeight();
			}
			userObject.setSize(new MoleculeSize(w,h));
				
		}	
		userObject.getGraph().refresh();
		userObject.setBounds(bounds);
	}
		
	public CellViewRenderer getRenderer() {
		return renderer;
	}
	
	public int getArcSize(int width, int height) {
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
	
	
	public class NucleicAcidFeatureRenderer extends VertexRenderer {
		
		
		public Dimension getPreferredSize() {
			Dimension d = super.getPreferredSize();
			d.width += d.width / 8;
			d.height += d.height / 2;
			return d;
		}
		
		
		public void paint(Graphics g) {
			
		
			setDoubleBuffered(true);
			setOpaque(false);
			
			int b = borderWidth;
			Graphics2D g2 = (Graphics2D) g;
			Dimension d = getSize();
			boolean tmp = selected;
						
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);
			g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
					RenderingHints.VALUE_TEXT_ANTIALIAS_ON);	
			
			g.setColor(super.getBackground());
									
			if(userObject.getColor()==null){
				if(userObject.isModulator()){
					gradientColor=Color.RED.darker();
				}else{				
					gradientColor=Color.BLUE;
				}
			}else{
				gradientColor=userObject.getColor();
			}
			
			if (gradientColor != null && !preview) {
				setOpaque(false);
				
				if(userObject.getMultimerCardinality()>1){		
					
					int x=15, y=15, z=10;
					if(d.width<80){
						x=10;
						y=10;
						z=5;
						
					}
				
					//System.out.println("222222INT="+d.width+","+d.height);
					g.setClip(0,0,d.width+20,d.height+20);
					if(userObject.isCopy()){
						
						
						int w=d.width;
						int h=d.height;						
						Rectangle rect=g2.getClipBounds();
						//System.out.println("(x,y)_0="+rect.getX()+","+rect.getY());			
						drawGeneSymbol(g, g2, x,y,h- (int) (5*b * 1.5), w-z,Color.GRAY);					
						g2.setClip(x, y, d.width- z, (d.height- (int) (5*b * 1.5))-(d.height- (int) (5*b * 1.5))/4);						

						
						//the background..........		
						GradientPaint p=new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true);
						drawGeneSymbol(g, g2, x,y,h- (int) (5*b * 1.5), w-z,p);	
						
						//System.out.println("(x,y)_1="+rect.getX()+","+rect.getY());
						rect=g2.getClipBounds();
						//System.out.println("(x,y)_2="+rect.getX()+","+rect.getY());
						g2.setClip(0, 0, d.width+20, d.height+20);
						rect=g2.getClipBounds();
						//System.out.println("(x,y)_3="+rect.getX()+","+rect.getY());
						
						//the foreground........
						int t=d.height;				
						drawGeneSymbol(g, g2, 0,0,t, w- (int) (b * 1.5),Color.gray);
						g2.setClip(0, -5, d.width- (int) (b * 1.5), t-t/3);	
						drawGeneSymbol(g, g2, 0,0,t-5, w- (int) (b * 1.5),p);						
						
						g2.setPaint(Color.WHITE);			
						g2.setClip(0, 0, d.width+20, d.height+20);
						
						Font f0=g.getFont();
						int inc=2*d.height/10+2;
						Font f1=new Font(f0.getFamily(),Font.BOLD,inc);
						g.setFont(f1);
												
						if(userObject.getCopyInstance()>=10){
							g.drawString(""+userObject.getCopyInstance(),d.width/2-8,13*d.height/14+1);
						}else{
							g.drawString(""+userObject.getCopyInstance(),d.width/2-4,13*d.height/14+1);
						}	
						f1=null;
						g.setFont(f0);
					
					}else{
						int w=d.width;
						int h=d.height;
						drawGeneSymbol(g, g2, x,y,h-(int)(5*b*1.5), w-z, new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true));
						g.setClip(0, 0, w+30, h+40);
						GradientPaint p=new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true);
						drawGeneSymbol(g, g2, 0,0,h-2, w,p);						
						
					}
					
					g2.setPaint(Color.GRAY);
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
					/*if (bordercolor != null) {
						g.setColor(bordercolor);
						g2.setStroke(new BasicStroke(b));
						g.drawRect(b / 2, b / 2,d.width - (int) (b * 1.5) - 1, d.height- (int) (b * 1.5));                                
					}*/
				}else{
					
					int x=15, y=15, z=10;
					if(d.width<80){
						x=10;
						y=10;
						z=5;
						
					}
				
					//System.out.println("222222INT="+d.width+","+d.height);
					g.setClip(-10,-10,d.width+20,d.height+20);
					if(userObject.isCopy()){
						int h=d.height;
						int w=d.width- (int) (b * 1.5);				
						drawGeneSymbol(g, g2, 0,0,h, w, Color.gray);						
						g2.setClip(0, -2, w, h-h/3);			
						drawGeneSymbol(g, g2, 0,0,h-3, w, new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true));						
						g2.setPaint(Color.WHITE);			
						g2.setClip(0, 0, d.width+10, d.height+10);
						
						Font f0=g.getFont();
						int inc=2*d.height/10+2;
						Font f1=new Font(f0.getFamily(),Font.BOLD,inc);
						g.setFont(f1);
						if(userObject.getCopyInstance()>=10){
							g.drawString(""+userObject.getCopyInstance(),d.width/2-8,13*d.height/14+2 );
						}else{
							g.drawString(""+userObject.getCopyInstance(),d.width/2-4,13*d.height/14+2 );
						}	
						f1=null;
						g.setFont(f0);
											
						
					}else{
						
					    int h=d.height- (int) (1*b * 1.5);
					    int w=d.width - (int) (b * 1.5) - 1;
					    GradientPaint color=new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, false);
						drawGeneSymbol(g, g2, 0,0,h, w, color);						
						g2.setPaint(Color.BLACK);					
						g.setClip(-20,-20,d.width+20,d.height+20);
						g2.setPaint(Color.GRAY);					
						if(userObject.getGraph().isShowInitialConcentration()){
							g.drawString(userObject.getInitialConcentration(),17,-1);
						}
					}
					
				}
				g2.setPaint(Color.BLACK);
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
			
					if(userObject.getRepresentingName().contains("-")){						
					
						StringTokenizer stTemp=new StringTokenizer(userObject.getRepresentingName(),"-");
						String tokenBefore=stTemp.nextToken().trim();
						int counter=1;
						String name="";
						while(stTemp.hasMoreTokens()){
							String token=stTemp.nextToken().trim();
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
							token=null;
						}	
						if(counter>1){	
							name+=tokenBefore;
						}
						
						if(userObject.getSize().getWidth()/name.length()<11){
							userObject.getSize().width+=11*name.length()+3;
							userObject.applySize();
						}
						int add=0;
						if(userObject.getMoleculeState()==MoleculeState.MODIFIED){
							add+=8;
						}
						if(userObject.isCopy()){						
							g.drawString(name,d.width/2-g.getFontMetrics().stringWidth(name)/2,d.height/3 +g.getFontMetrics().getHeight()/5+add);						
							
						}else{
						
							g.drawString(name,d.width/2-g.getFontMetrics().stringWidth(name)/2,d.height/2 +g.getFontMetrics().getHeight()/5+add);
						
						}
						
						stTemp=null;
						tokenBefore=null;
						name=null;
					}else{	
						
						if(userObject.getSize().getWidth()/userObject.getRepresentingName().length()<11){
							userObject.getSize().width+=10*userObject.getRepresentingName().length();
							userObject.applySize();
						}
						int add=0;
						if(userObject.getMoleculeState()==MoleculeState.MODIFIED){
							add+=8;
						}
						Font f0=g.getFont();
						Font f1=new Font(f0.getFamily(),Font.BOLD,f0.getSize());
						g.setColor(Color.black);
						g.setFont(f1);
						if(userObject.isCopy()){
							g.drawString(userObject.getRepresentingName(),d.width/2-g.getFontMetrics().stringWidth(userObject.getRepresentingName())/2,d.height/3 +g.getFontMetrics().getHeight()/5+2+add);							
						}else{
							g.drawString(userObject.getRepresentingName(),d.width/2-g.getFontMetrics().stringWidth(userObject.getRepresentingName())/2,d.height/2 +g.getFontMetrics().getHeight()/5+add);
						}
					f1=null;	
					}
				}	
			}
			
			try {
				setBorder(null);
				setOpaque(false);
				selected = false;
				super.paint(g);
			} finally {
				selected = tmp;
			}
			bordercolor=Color.BLACK;
			setBorder(BorderFactory.createRaisedBevelBorder());
			/*if (bordercolor != null) {
				g.setColor(bordercolor);
				g2.setStroke(new BasicStroke(b));
				g.drawRect(0, 0,d.width - (int) (b * 1.5) - 1, d.height- (int) (1*b * 1.5));                         
			}*/
			
			if(userObject.getMoleculeState()==MoleculeState.MODIFIED){
				g2.setPaint(Color.BLACK);
				g2.setStroke(new BasicStroke(1));
				g.setClip(-5,-5,d.width+340,d.height+40);	
				g.drawOval(d.width- (int) (b * 1.5), -3,16,16);
				g.drawString("m",d.width- (int) (b * 1.5)+3,10);
				
				if(userObject.getModificationSites()!=null){
					FontRenderContext frc = new FontRenderContext(null, false, false);	
					int len=(int)g.getFont().getStringBounds(userObject.getModificationSites(), frc).getWidth();
					int min_0=len;
					if(userObject.getModificationSites().length()<2){
						min_0+=2;
					}else if(len>=d.width-20){
						min_0-=36*(len-d.width)/37+userObject.getNumberOfModifSites()*2+5;						
					}
					if(len>=d.width-10){
						Utils.debugMsg("\n\n*************************before="+userObject.getSize().width);
						if(Math.abs(userObject.getSize().width-(d.width+(len-d.width)/4))>5){
							userObject.getSize().width=d.width+(len-d.width)/4;	
							userObject.applySize();		
						}				
					}
					/*int min_0=userObject.getModificationSites().length()*5+d.width/(userObject.getNumberOfModifSites()*6);//(10-userObject.getNumberOfModifSites())+d.width/10;
					min_0+=(userObject.getModificationSites().length()/userObject.getNumberOfModifSites())*userObject.getModificationSites().length()/userObject.getNumberOfModifSites()/2+userObject.getNumberOfModifSites()*(userObject.getNumberOfModifSites()/3-userObject.getModificationSites().length()/2-2);
					//System.out.println("\n\nNUCLEIC ACID FEATURE: adding="+d.width/(userObject.getNumberOfModifSites()*40)+"sites="+userObject.getNumberOfModifSites()+", length="+userObject.getModificationSites().length()+"**MACROMOLECULE**len="+min_0+"\n****d.width="+d.width);
					if(min_0>140){
						min_0+=5;
					}
					if(d.width-min_0<=20){
						min_0+=2;
						//System.out.println("\n\nNUCLEIC ACID FEATURE: min_0="+min_0+",after adding: "+(userObject.getModificationSites().length()-5));
					}
					min_0+=userObject.getModificationSites().length()/userObject.getNumberOfModifSites();
					if(userObject.getModificationSites().length()/userObject.getNumberOfModifSites()>=3){
						if(min_0+2*userObject.getModificationSites().length()-d.width<=31){
							min_0+=userObject.getModificationSites().length();//+userObject.getModificationSites().length()/userObject.getNumberOfModifSites();
							System.out.println("\n\nNUCLEIC ACID FEATURE: HEREEEEEEE");
						}
						
					}
					
					if(min_0>=d.width+2){
						//System.out.println("\n\nNUCLEIC ACID FEATURE: ****before="+userObject.getSize().width);						
						if(userObject.getSize().width<=d.width){
							userObject.getSize().width+=17;		
							min_0+=17;
						}	
						
						userObject.applySize();					
					}*/
					Font f0=g.getFont();
					//int inc=3*d.height/11;
					Font f1=new Font(f0.getFamily(),Font.PLAIN,10);
					g.setFont(f1);	
					g2.setPaint(Color.black);
					//g2.setPaint(new Color(115,115,115));//96,123,139));//131,139,131));//0,128,128));//85,85,85));
					g.drawString(userObject.getModificationSites(),d.width-min_0,12);	
					f1=null;
					g.setFont(f0);
					/*//Font f0=g.getFont();
					//int inc=11;
					//Font f1=new Font(f0.getFamily(),Font.PLAIN,inc);
					//g.setFont(f1);	
					g2.setPaint(new Color(115,115,115));//96,123,139));//131,139,131));//0,128,128));//85,85,85));
*/					
					//f1=null;
					//g.setFont(f0);
					
				//}				
					/*int min_0=userObject.getModificationSites().length()*(10-userObject.getNumberOfModifSites())+d.width/10;			
					if(min_0>150){
						min_0+=30;
					}
					if(min_0>d.width){					
						userObject.getSize().width+=20;
						userObject.applySize();
					}
					if(userObject.getModificationSites()!=null){
						g2.setPaint(new Color(135,135,135));
						g.drawString(userObject.getModificationSites(),d.width-min_0,12);	
					}		*/
				}
				g2.setPaint(Color.black);
			}
			
			Position pos=userObject.getPosition();
			
			if(pos!=null){
				
				Rectangle2D bnds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
				if(bnds!=null){
					
					double xc=pos.getX();
					double yc=pos.getY();
					double xx=bnds.getX();
					double yy=bnds.getY();
					ENucleicAcidFeatureCell iv=(ENucleicAcidFeatureCell)cell;
					iv.setX(xx);
					iv.setY(yy);
					pos.setX(xx);
					pos.setY(yy);
				}
			}			
		}


		/**
		 * @param g
		 * @param g2
		 * @param h
		 * @param w
		 * @param color
		 */
		private void drawGeneSymbol(Graphics g, Graphics2D g2,int x, int y, int h, int w, Paint color) {
			g2.setPaint(color);				
			g.fillRoundRect(x, y,w,h ,15,15);
			g2.setPaint(Color.BLACK);						
			g.drawRoundRect(x, y,w,h ,15,15);
			g2.setPaint(color);
			g.setClip(x, y, w+20, h-h/4); 
			g.fillRect(x, y, w, h);
			g2.setPaint(Color.BLACK);						
			g.drawRect(x, y, w, h);
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


}
