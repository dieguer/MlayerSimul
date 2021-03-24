function [gmm se condition] = GMMA(Y,X,gmm0)
%% Calculates GMM1 point estimator and standard error as described in `Two-way model for gravity' (Jochmans 2016).
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
d_error = cell(d,1); for k=1:d, d_error{k} = error.*X{k}; end; % derivative of disturbance
% averages
error_i = sum(error,2);  
error_j = sum(error,1); 
d_error_i =  cell(d,1); for k=1:d, d_error_i{k} = sum(d_error{k},2); end; 
d_error_j =  cell(d,1); for k=1:d, d_error_j{k} = sum(d_error{k},1); end; 
m_error = sum(sum(error)); for k=1:d, m_derror{k} = sum(sum(d_error{k})); end
% score vector
S = zeros(d,1); for k=1:d, S(k) = sum(sum(error.*X{k}))*sum(sum(error)) - sum(sum((error_i*error_j).*X{k})); end
% Jacobian matrix
H = zeros(d,d); 
for k=1:d,
    for j=1:d,
        H(k,j) = sum(sum(X{k}.*error.*(X{j}*m_error+m_derror{j}) - X{k}.*(error_i*d_error_j{j}+d_error_i{j}*error_j)));
    end
end
H = -H;
% objective function
criterion = -  S'*S; 
score     = -2*H'*S;
Hessian   = -2*H'*H;

%% Estimation of the asymptotic variance of the moment conditions
function [mVar] = Vmatrix(psi,Y,X)
% dimensions
[n m] =   size(Y  ); % sample size
[  d] = length(psi); % number of regressors
% variable definitions
index = zeros(n,m); for k=1:d, index = index+X{k}*psi(k); end; % linear index 
phi   = exp(index); % exponential transform  
error =     Y./phi; % disturbance

% components for asyvar of moments
mm_u = sum(sum(error));
for k=1:d, mm_ux{k} = sum(sum(error.*X{k})); end
for i=1:n,
    for k=1:d, 
        m_xu{k}(i,1) =  sum(sum((ones(n,1)*X{k}(i,:)).*error));
        m_ux{k}(1,i) =  sum(sum((X{k}(:,i)*ones(1,m)).*error));
    end
    for j=1:m,
        m_uu(i,j) = sum(sum(error(:,j)*error(i,:)));
        for k=1:d,
           m_xuu{k}(i,j) = sum(sum((ones(n,1)*X{k}(i,:)).*(ones(n,1)*error(i,:)).*(error(:,j)*ones(1,m))));
           m_uux{k}(i,j) = sum(sum((X{k}(:,j)*ones(1,m)).*(ones(n,1)*error(i,:)).*(error(:,j)*ones(1,m))));
           m_uxu{k}(i,j) = sum(sum( X{k}                .*(ones(n,1)*error(i,:)).*(error(:,j)*ones(1,m))));
        end
    end
end
% collecting blocks for moment asyvar
for k=1:d,
    xi{k} = X{k}.*error*(mm_u)-X{k}.*m_uu-error.*(m_xu{k}*ones(1,m))+m_xuu{k}-error.*(ones(n,1)*m_ux{k})+m_uux{k}+error*mm_ux{k}-m_uxu{k};
    xi{k} = 4*xi{k}/(n-1)^2;
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