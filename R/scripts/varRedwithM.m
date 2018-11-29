%下面的程序计算了m与真实方差平均减小率之间的关系
%计算公式为:
%\begin{equation}
% 4 \int_0^{0.5} \int_0^{0.5} \frac{1+\rho_1-2\rho_2}{(m+1)(1+\rho_1)+2(m^2-1)\rho_2} d\rho_1 d\rho_2 < \alpha\%
%\end{equation}
warning off
syms a b m
for i = 2:1:20
	f = sym('(1+a-2*b)/((m+1)*(1+a)+2*(m^2-1)*b)');
	f3=subs(f, 'm', i);
	r = 4* int(int(f3, a, 0, 0.5), b, 0, 0.5);
	a1 = subs(r);
	fprintf('%d, %8.5f\n', i, a1)
end
warning on