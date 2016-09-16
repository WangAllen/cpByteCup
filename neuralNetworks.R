library(FCNN4R)
inp <- c(0,0,1,1,0,1,0,1)
dim(inp) <- c(4,2)
outp <- c(0,1,1,0)
dim(outp) <- c(4,1)
net <- mlp_net(c(2,6,1))
net <- mlp_rnd_weights(net)
mlp_plot(net, T)
tol <- 0.5e-4
#netmse <- mlp_teach_bp(net, inp, outp, tol_level = tol, max_epochs = 500)
netmse <- mlp_teach_rprop(net, inp, outp, tol_level = tol, max_epochs = 500, report_freq = 10)
net <- netmse$net

plot(netmse$mse, type = 'l')
mlp_plot(net, T)
if(mlp_mse(net, inp, outp) <= tol) {
  net <- mlp_prune_obs(net, inp, outp, tol_level = tol, max_reteach_epochs = 500, report = T)[[1]]
  mlp_plot(net, T)
}



