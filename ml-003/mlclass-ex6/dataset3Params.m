function [C, sigma] = dataset3Params(X, y, Xval, yval)
%EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
% sigma = 0.3;
sigma = 0.1;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

C_vals     = [.01 .03 .1 .3 1 3 10 30];
sigma_vals = [.01 .03 .1 .3 1 3 10 30];
model_errors = zeros(length(sigma_vals), length(C_vals));
cv_errors = zeros(length(sigma_vals), length(C_vals));

for i = 1:length(C_vals)
    for j = 1:length(sigma_vals)
        model= svmTrain(X, y, C_vals(i), @(x1, x2) gaussianKernel(x1, x2, ...
                                                          sigma_vals(j)));
        predictions = svmPredict(model, Xval);
        cv_errors(i,j) = mean(double(predictions ~= yval));
    end
end

fprintf('CV erros\n');
fprintf('________\n');
for i = 1:length(C_vals)
    for j = 1:length(sigma_vals)
        if j ~= i
            fprintf('\t');
        end
        fprintf('%d', cv_errors(i,j));
    end
    fprintf('\n');
end

[r, c] = find(cv_errors==min(min(cv_errors)));

fprintf('Best C,sigma: %f %f\n', C_vals(c), sigma_vals(r));

% =========================================================================

end
