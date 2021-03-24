function [gmm se condition] = GMMB(Y,X,gmm0)
%% Calculates GMM2 point estimator and standard error as described in `Two-way model for gravity' (Jochmans 2016).
% inputs: Y = n-by-n array of outcomes; X = d-dimensional cell of n-by-n matrices of regressors; initial value (gmm0).
% outputs: point estimate (gmm), standard error (se), and convergence indicator (0 if algorithm converged).


% Minimization of GMM problem by Newton's method, starting at gmm0
[gmm, condition, numiter, S] = Newton(@QuadraticForm,gmm0,Y,X); 
% Estimation of variance of moment conditions
[n m] = size(Y); nn = nchoosek(n,2); mm = nchoosek(m,2); rho = nn*mm; J = inv(S/rho);
[V] = Vmatrix(gmm,Y,X);  
% Construction of standard errors
Upsilon = J*V*J; se = sqrt(abs(diag(Upsilon))/(n*m));

%% Evaluation of GMM problem at fixed parameter value psi
function [criterion score Hessian H] = QuadraticForm(psi,Y,X)
% dimensions
[n m] =   size(Y  ); % sample size
[  d] = length(psi); % number of regressors
% variable definitions
index = zeros(n,m); for k=1:d, index = index+X{k}*psi(k); end; % linear index 
phi   = exp(index); % exponential transform  
error =     Y./phi; % disturbance
% construction via loop
for i=1:n,
    for j=1:m,
        block1(i,j) = sum(sum((phi(:,j)*phi(i,:)).*  Y));
        block2(i,j) = sum(sum((  Y(:,j)*  Y(i,:)).*phi));
        for k=1:d 
            dblock1{k}(i,j) = sum(sum((phi(:,j)*phi(i,:)).*  Y.*(X{k}(:,j)*ones(1,m)+ones(n,1)*X{k}(i,:)))); 
            dblock2{k}(i,j) = sum(sum((  Y(:,j)*  Y(i,:)).*phi.* X{k}                                    ));
        end
    end
end
% score
for k=1:d, S(k,1) = sum(sum(X{k}.*(Y.*block1-phi.*block2))); end
% Jacobian
for k=1:d,
    for j=1:d,
        H(k,j) = sum(sum(X{k}.*(Y.*dblock1{j}-phi.*X{j}.*block2-phi.*dblock2{j})));
    end
end
% objective function
criterion  = -  S'*S; 
score      = -2*H'*S;
Hessian    = -2*H'*H;


%% Estimation of the asymptotic variance of the moment conditions
function [mVar] = Vmatrix(psi,Y,X)
% dimensions
[n m] =   size(Y  ); % sample size
[  d] = length(psi); % number of regressors
% variable definitions
index = zeros(n,m); for k=1:d, index = index+X{k}*psi(k); end; % linear index 
phi   = exp(index); % exponential transform  


% components for asyvar of moments
for i=1:n,
    for j=1:m,
        for k=1:d,
            ma1{k}(i,j) = sum(sum(Y.*(phi(:,j)*phi(i,:))                       ));
            ma2{k}(i,j) = sum(sum(Y.*(phi(:,j)*phi(i,:)).*(ones(n,1)*X{k}(i,:))));
            ma3{k}(i,j) = sum(sum(Y.*(phi(:,j)*phi(i,:)).*(X{k}(:,j)*ones(1,m))));
            ma4{k}(i,j) = sum(sum(Y.*(phi(:,j)*phi(i,:)).* X{k}                ));
            
            mb1{k}(i,j) = sum(sum(phi.*(Y(:,j)*Y(i,:))                       ));
            mb2{k}(i,j) = sum(sum(phi.*(Y(:,j)*Y(i,:)).*(ones(n,1)*X{k}(i,:))));
            mb3{k}(i,j) = sum(sum(phi.*(Y(:,j)*Y(i,:)).*(X{k}(:,j)*ones(1,m))));
            mb4{k}(i,j) = sum(sum(phi.*(Y(:,j)*Y(i,:)).* X{k}                ));
        end
    end
end

% collecting blocks for moment asyvar
for k=1:d,
    xi{k} = X{k}.*Y.*ma1{k}-Y.*ma2{k}-Y.*ma3{k}+Y.*ma4{k}-X{k}.*phi.*mb1{k}+phi.*mb2{k}+phi.*mb3{k}-phi.*mb4{k};
    %xi{k} = 4*xi{k}/(n-1)^2;
    xi{k} = 4*xi{k}/(n)^2;
end

for k=1:d,
    for j=1:d,
        mVar(k,j) = mean(mean(xi{k}.*xi{j})); 
    end
end

%% Newton algorithm used for optimization
function [x condition it J]=Newton(FUN,x,varargin) % varargout
% maximises FUN, starting at x by Newton-Raphson method
tol=1e-5; maxit=100; smalleststep=.5^20;
it=1; condition=1; improvement=1; k=length(x);
[f g H J] =feval(FUN,x,varargin{:}); %varargout
while it<=maxit && condition==1 && improvement==1;
    [s1 s2]=size(H); if s1==s2 && s2>1 d=-pinv(H)*g; else d=-g./H; end      
    step=1; improvement=0;
    while step>=smalleststep && improvement==0;
        [ff gg HH JJ] =feval(FUN,x+step*d,varargin{:}); %varargout
        bounded = sum(sum(isnan(HH)))==0 & sum(sum(isinf(HH)))==0;
        if (ff-f)/abs(f)>=-1e-5 & bounded==1;
            improvement=1; condition=sqrt(step*step*(d'*d))>tol & (ff-f)>tol;
            x=x+step*d; f=ff; g=gg; H=HH; J = JJ;
        else
            step=step/2;
        end
    end
    it=it+1;
end
it=it-1;