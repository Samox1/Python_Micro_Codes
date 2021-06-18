function w=regresja_logistyczna(x,y)
X=[ones(size(x,1),1),x]; 
w=rand(size(X,2),1)/1000;

w_old=w+inf;
while norm(w-w_old)>0.00001
    w_old=w;
    p=exp(X*w)./(1+exp(X*w));
    V=diag(p.*(1-p));
    w=w+(X'*V*X)\X'*(y-p);
end





